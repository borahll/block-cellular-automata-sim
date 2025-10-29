module BlockAutomaton.Server
  ( Automaton
  , startAutomaton
  , stepAutomaton
  , observeAutomaton
  ) where

import BlockAutomaton.Rules
  ( Rules(..)
  , MargolusPos(..)
  , margolusInitial
  , margolusShift
  )
import Control.Concurrent (Chan, newChan, readChan, writeChan, forkIO)
import Control.Monad (forM_, forever, replicateM_, void, when)
import GenServer
  ( Server, ReplyChan
  , spawn, sendTo, receive
  , requestReply, reply
  )

data Automaton obs = Automaton (Server (AutomatonMsg obs))
data AutomatonMsg obs = MsgStep | MsgObserve (ReplyChan [[obs]])

stepAutomaton :: Automaton obs -> IO ()
stepAutomaton (Automaton srv) = sendTo srv MsgStep

observeAutomaton :: Automaton obs -> IO [[obs]]
observeAutomaton (Automaton srv) = requestReply srv MsgObserve

startAutomaton :: Rules state obs -> Int -> Int -> IO (Automaton obs)
startAutomaton rules h w = do
  when (odd h || odd w) $
    ioError (userError $
      "BlockAutomaton: cannot partition a "
      ++ show h ++ "x" ++ show w
      ++ " torus into disjoint 2x2 Margolus blocks. Both dimensions must be even.")

  inG <- grid h w (const newChan)
  nG  <- grid h w (const newChan)
  sG  <- grid h w (const newChan)
  wG  <- grid h w (const newChan)
  eG  <- grid h w (const newChan)

  chs <- grid h w $ \(x,y) -> do
    inbox <- at inG (x,y)
    ni    <- at nG  (x,y)
    si    <- at sG  (x,y)
    wi    <- at wG  (x,y)
    ei    <- at eG  (x,y)
    pure (CellCh inbox ni si wi ei)

  acks <- newChan

  let chOf (x,y) = (chs !! x) !! y

  forM_ [0..h-1] $ \x ->
    forM_ [0..w-1] $ \y -> do
      let s0  = rulesInitial rules (x,y)
          pos = margolusInitial (x,y)
      void $ forkIO $
        cellThread rules h w (x,y) chOf (chOf (x,y)) s0 pos acks

  let st = AutoState rules h w inG acks
  Automaton <$> spawn (serverLoop st)

type H = Int
type W = Int
type Coord = (Int,Int)

wrap :: H -> W -> Coord -> Coord
wrap h w (i,j) = ((i+h) `mod` h, (j+w) `mod` w)

north, south, west, east :: H -> W -> Coord -> Coord
north h w (i,j) = wrap h w (i-1, j)
south h w (i,j) = wrap h w (i+1, j)
west  h w (i,j) = wrap h w (i, j-1)
east  h w (i,j) = wrap h w (i, j+1)

data Phase = PhaseA | PhaseB deriving (Eq, Show)
data CellMsg s
  = DoPhase Phase
  | Pull (Chan s)

data PairMsg s = Req !s | Res !s

data CellCh s = CellCh
  { inbox  :: Chan (CellMsg s)
  , nInbox :: Chan (PairMsg s)
  , sInbox :: Chan (PairMsg s)
  , wInbox :: Chan (PairMsg s)
  , eInbox :: Chan (PairMsg s)
  }


data Ack = Ack

data AutoState s o = AutoState
  { aRules :: Rules s o
  , aH     :: !H
  , aW     :: !W
  , aIn    :: [[Chan (CellMsg s)]]
  , aAcks  :: Chan Ack
  }

broadcast :: msg -> [[Chan msg]] -> IO ()
broadcast m = mapM_ (mapM_ (`writeChan` m))

waitNAcks :: Chan Ack -> Int -> IO ()
waitNAcks c n = replicateM_ n (readChan c)

grid :: Int -> Int -> (Coord -> IO a) -> IO [[a]]
grid h w f = mapM (\i -> mapM (\j -> f (i,j)) [0..w-1]) [0..h-1]

at :: [[a]] -> Coord -> IO a
at m (i,j) = pure ((m !! i) !! j)

cellThread
  :: Rules s o -> H -> W -> Coord
  -> (Coord -> CellCh s) -> CellCh s
  -> s -> MargolusPos
  -> Chan Ack -> IO ()
cellThread rules h w (i,j) chOf myCh s0 pos0 acks = loop s0 pos0
  where
    loop s pos = do
      msg <- readChan (inbox myCh)
      case msg of
        Pull rch -> do
          writeChan rch s
          loop s pos

        DoPhase PhaseA ->
          if pos == UL || pos == LL
            then do
              let pCh = chOf (east h w (i,j))
              writeChan (wInbox pCh) (Req s)
              Res sP <- readChan (eInbox myCh)
              let (s', sP') = rulesInteract rules (s, sP)
              writeChan (wInbox pCh) (Res sP')
              writeChan acks Ack
              loop s' pos
            else do

              Req sW <- readChan (wInbox myCh)
              writeChan (eInbox (chOf (west h w (i,j)))) (Res s)
              Res sNew <- readChan (wInbox myCh)
              writeChan acks Ack
              loop sNew pos


        DoPhase PhaseB ->
          if pos == UL || pos == UR
            then do
              let pCh = chOf (south h w (i,j))
              writeChan (nInbox pCh) (Req s)
              Res sP <- readChan (sInbox myCh)
              let (s', sP') = rulesInteract rules (s, sP)
              writeChan (nInbox pCh) (Res sP')
              writeChan acks Ack
              loop s' (margolusShift pos)
            else do
              Req sN <- readChan (nInbox myCh)
              writeChan (sInbox (chOf (north h w (i,j)))) (Res s)
              Res sNew <- readChan (nInbox myCh)
              writeChan acks Ack
              loop sNew (margolusShift pos)

serverLoop :: AutoState s o -> Chan (AutomatonMsg o) -> IO ()
serverLoop st inbox = forever $ do
  msg <- receive inbox
  case msg of
    MsgStep -> do
      let h = aH st; w = aW st
      broadcast (DoPhase PhaseA) (aIn st)
      waitNAcks (aAcks st) (h*w)
      broadcast (DoPhase PhaseB) (aIn st)
      waitNAcks (aAcks st) (h*w)

    MsgObserve rc -> do
      rows <- mapM
        (\row -> mapM
          (\cellIn -> do r <- newChan
                         writeChan cellIn (Pull r)
                         s <- readChan r
                         pure s)
          row)
        (aIn st)
      reply rc (map (map (rulesObserve (aRules st))) rows)
