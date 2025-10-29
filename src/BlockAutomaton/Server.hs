module BlockAutomaton.Server
  ( Automaton,
    startAutomaton,
    stepAutomaton,
    observeAutomaton,
  )
where

import BlockAutomaton.Rules
import Control.Concurrent (Chan, readChan, writeChan, forkIO, newChan)
import Control.Monad (forM_, forever, replicateM_)
import Data.IORef
import GenServer
  ( Server, ReplyChan
  , spawn, send, sendTo, receive
  , requestReply, reply
  )

newtype Automaton obs = Automaton (Server (AutomatonMsg obs))

data AutomatonMsg obs
  = MsgStep
  | MsgObserve (ReplyChan [[obs]])

type H = Int
type W = Int
type Coord = (Int,Int)
wrap  :: H -> W -> Coord -> Coord
north :: H -> W -> Coord -> Coord
south :: H -> W -> Coord -> Coord
west  :: H -> W -> Coord -> Coord
east  :: H -> W -> Coord -> Coord
wrap h w (i,j) = ((i+h) `mod` h, (j+w) `mod` w)
north h w (i,j) = wrap h w (i-1,j)
south h w (i,j) = wrap h w (i+1,j)
west  h w (i,j) = wrap h w (i,j-1)
east  h w (i,j) = wrap h w (i,j+1)

data SupMsg = StepMsg !Int !Phase
data Phase  = PhaseA | PhaseB deriving (Eq,Show)
data PairMsg s = Req !s | Res !s

data CellCh s = CellCh
  { supInbox :: Chan SupMsg
  , nInbox   :: Chan (PairMsg s)
  , sInbox   :: Chan (PairMsg s)
  , wInbox   :: Chan (PairMsg s)
  , eInbox   :: Chan (PairMsg s)
  }

type CellState s = IORef s
data Ack = Ack

-- Drive pairing by MargolusPos (UL/UR/LL/LR), not parity.
-- PhaseA: LEFT cells (UL,LL) initiate with EAST; right cells respond.
-- PhaseB: TOP  cells (UL,UR) initiate with SOUTH; bottom cells respond.
-- After PhaseB, shift pos := margolusShift pos (exactly once per step).
cellThread
  :: Rules s o -> H -> W -> Coord
  -> (Coord -> CellCh s)      -- ^ channel lookup by coord
  -> CellCh s                 -- ^ my inboxes
  -> IORef s                  -- ^ my cell state
  -> IORef MargolusPos        -- ^ my Margolus position (UL/UR/LL/LR)
  -> Chan Ack                 -- ^ supervisor ack channel
  -> IO ()
cellThread rules h w (i,j) chOf myCh sRef posRef ackOut = forever $ do
  StepMsg _t phase <- readChan (supInbox myCh)
  pos <- readIORef posRef

  case phase of
    -- Phase A: UL & LL initiate to the EAST neighbor, UR & LR respond.
    PhaseA ->
      if pos == UL || pos == LL
        then do
          sSelf <- readIORef sRef
          let p   = east h w (i,j)
              pCh = chOf p
          -- initiator sends its state to partner's WEST inbox
          writeChan (wInbox pCh) (Req sSelf)
          -- initiator receives partner's state from its own EAST inbox
          Res sP <- readChan (eInbox myCh)
          let (sSelf', sP') = rulesInteract rules (sSelf, sP)
          writeIORef sRef sSelf'
          -- initiator sends partner's updated state back to partner's WEST inbox
          writeChan (wInbox pCh) (Res sP')
          -- done with Phase A
          writeChan ackOut Ack

        else do
          -- responder: wait on WEST inbox for partner's request
          Req _sOther <- readChan (wInbox myCh)
          sSelf <- readIORef sRef
          -- send my current state to initiator's EAST inbox
          writeChan (eInbox (chOf (west h w (i,j)))) (Res sSelf)
          -- receive my updated state back on my WEST inbox
          Res sSelf' <- readChan (wInbox myCh)
          writeIORef sRef sSelf'
          writeChan ackOut Ack

    -- Phase B: UL & UR initiate to the SOUTH neighbor, LL & LR respond.
    PhaseB ->
      if pos == UL || pos == UR
        then do
          sSelf <- readIORef sRef
          let p   = south h w (i,j)
              pCh = chOf p
          -- initiator sends to partner's NORTH inbox
          writeChan (nInbox pCh) (Req sSelf)
          -- initiator receives partner's state from its own SOUTH inbox
          Res sP <- readChan (sInbox myCh)
          let (sSelf', sP') = rulesInteract rules (sSelf, sP)
          writeIORef sRef sSelf'
          -- initiator sends partner's updated state back to partner's NORTH inbox
          writeChan (nInbox pCh) (Res sP')
          -- shift Margolus tiling ONCE per step, after PhaseB
          writeIORef posRef (margolusShift pos)
          writeChan ackOut Ack

        else do
          -- responder: wait on NORTH inbox
          Req _sOther <- readChan (nInbox myCh)
          sSelf <- readIORef sRef
          -- send my current state to initiator's SOUTH inbox
          writeChan (sInbox (chOf (north h w (i,j)))) (Res sSelf)
          -- receive my updated state back on my NORTH inbox
          Res sSelf' <- readChan (nInbox myCh)
          writeIORef sRef sSelf'
          -- shift Margolus tiling after PhaseB
          writeIORef posRef (margolusShift pos)
          writeChan ackOut Ack




data AutoState s o = AutoState
  { aRules  :: Rules s o
  , aH      :: !H
  , aW      :: !W
  , aT      :: IORef Int
  , aSupIn  :: [[Chan SupMsg]]
  , aAcks   :: Chan Ack
  , aStates :: [[CellState s]]
  }

startAutomaton :: Rules state obs -> Int -> Int -> IO (Automaton obs)
startAutomaton rules h w = do
  supG <- grid h w (const newChan)
  nG   <- grid h w (const newChan)
  sG   <- grid h w (const newChan)
  wG   <- grid h w (const newChan)
  eG   <- grid h w (const newChan)

  chs <- grid h w $ \(x,y) -> do
    sup <- at supG (x,y); ni <- at nG (x,y); si <- at sG (x,y); wi <- at wG (x,y); ei <- at eG (x,y)
    return (CellCh sup ni si wi ei)

  srefs  <- grid h w $ \(x,y) -> newIORef (rulesInitial rules (x,y))
  posRefs <- grid h w $ \(x,y) -> newIORef (margolusInitial (x,y))  -- <-- NEW

  acks <- newChan
  tVar <- newIORef 0

  let chOf (x,y) = (chs !! x) !! y

  -- spawn all cell threads
  forM_ [0..h-1] $ \x ->
    forM_ [0..w-1] $ \y -> do
      let myCh   = chOf (x,y)
          myRef  = (srefs  !! x) !! y
          posRef = (posRefs !! x) !! y
      _ <- forkIO (cellThread rules h w (x,y) chOf myCh myRef posRef acks)
      pure ()

  let st = AutoState rules h w tVar supG acks srefs
  srv <- spawn (serverLoop st)
  return (Automaton srv)

stepAutomaton :: Automaton obs -> IO ()
stepAutomaton (Automaton srv) = sendTo srv MsgStep

observeAutomaton :: Automaton obs -> IO [[obs]]
observeAutomaton (Automaton srv) = requestReply srv MsgObserve

serverLoop :: AutoState s o -> Chan (AutomatonMsg o) -> IO ()
serverLoop st inbox = forever $ do
  msg <- receive inbox
  case msg of
    MsgStep -> do
      t <- readIORef (aT st)
      broadcast (StepMsg t PhaseA) (aSupIn st)
      waitNAcks (aAcks st) (aH st * aW st)
      broadcast (StepMsg t PhaseB) (aSupIn st)
      waitNAcks (aAcks st) (aH st * aW st)
      atomicModifyIORef' (aT st) (\x -> (x+1, ()))

    MsgObserve rc -> do
      rows <- mapM (mapM readIORef) (aStates st)
      let obs = map (map (rulesObserve (aRules st))) rows
      reply rc obs

broadcast :: SupMsg -> [[Chan SupMsg]] -> IO ()
broadcast m g = forM_ g $ \row -> forM_ row $ \c -> writeChan c m

waitNAcks :: Chan Ack -> Int -> IO ()
waitNAcks c n = replicateM_ n (readChan c >> return ())

grid :: Int -> Int -> (Coord -> IO a) -> IO [[a]]
grid h w f = mapM (\i -> mapM (\j -> f (i,j)) [0..w-1]) [0..h-1]

at :: [[a]] -> Coord -> IO a
at m (i,j) = return ((m !! i) !! j)