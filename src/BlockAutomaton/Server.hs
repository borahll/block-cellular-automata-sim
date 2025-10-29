module BlockAutomaton.Server
  ( Automaton
  , startAutomaton
  , stepAutomaton
  , observeAutomaton
  ) where

import BlockAutomaton.Rules
import Control.Concurrent (Chan, readChan, writeChan, forkIO, newChan)
import Control.Monad (forM_, forever, replicateM_)
import Data.IORef
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
  supG <- grid h w (const newChan)
  nG   <- grid h w (const newChan)
  sG   <- grid h w (const newChan)
  wG   <- grid h w (const newChan)
  eG   <- grid h w (const newChan)

  chs <- grid h w $ \(x,y) -> do
    sup <- at supG (x,y); ni <- at nG (x,y); si <- at sG (x,y); wi <- at wG (x,y); ei <- at eG (x,y)
    pure (CellCh sup ni si wi ei)

  srefs   <- grid h w $ \(x,y) -> newIORef (rulesInitial rules (x,y))
  posRefs <- grid h w $ \(x,y) -> newIORef (margolusInitial (x,y))
  acks    <- newChan
  tVar    <- newIORef 0

  let chOf (x,y) = (chs   !! x) !! y
      sRef (x,y) = (srefs !! x) !! y
      pRef (x,y) = (posRefs !! x) !! y

  forM_ [0..h-1] $ \x ->
    forM_ [0..w-1] $ \y ->
      void $ forkIO $ cellThread rules h w (x,y) chOf (chOf (x,y)) (sRef (x,y)) (pRef (x,y)) acks

  let st = AutoState rules h w tVar supG acks srefs
  Automaton <$> spawn (serverLoop st)

type H = Int; type W = Int; type Coord = (Int,Int)
wrap h w (i,j) = ((i+h) `mod` h, (j+w) `mod` w)
north h w (i,j) = wrap h w (i-1,j); south h w (i,j) = wrap h w (i+1,j)
west  h w (i,j) = wrap h w (i,j-1); east  h w (i,j) = wrap h w (i,j+1)

data SupMsg = StepMsg !Int !Phase
data Phase  = PhaseA | PhaseB deriving (Eq,Show)
data PairMsg s = Req !s | Res !s
data CellCh s = CellCh { supInbox :: Chan SupMsg, nInbox :: Chan (PairMsg s)
                       , sInbox :: Chan (PairMsg s), wInbox :: Chan (PairMsg s)
                       , eInbox :: Chan (PairMsg s) }
type CellState s = IORef s
data Ack = Ack

data AutoState s o = AutoState
  { aRules  :: Rules s o
  , aH      :: !H
  , aW      :: !W
  , aT      :: IORef Int
  , aSupIn  :: [[Chan SupMsg]]
  , aAcks   :: Chan Ack
  , aStates :: [[CellState s]]
  }

cellThread
  :: Rules s o -> H -> W -> Coord
  -> (Coord -> CellCh s) -> CellCh s -> IORef s -> IORef MargolusPos
  -> Chan Ack -> IO ()
cellThread rules h w (i,j) chOf myCh sRef posRef acks = forever $ do
  StepMsg _ phase <- readChan (supInbox myCh)
  pos <- readIORef posRef
  case phase of
    PhaseA ->
      if pos == UL || pos == LL
        then do
          s <- readIORef sRef
          let pCh = chOf (east h w (i,j))
          writeChan (wInbox pCh) (Req s)
          Res sP <- readChan (eInbox myCh)
          let (s', sP') = rulesInteract rules (s, sP)
          writeIORef sRef s'
          writeChan (wInbox pCh) (Res sP')
          writeChan acks Ack
        else do
          Req _ <- readChan (wInbox myCh)
          s <- readIORef sRef
          writeChan (eInbox (chOf (west h w (i,j)))) (Res s)
          Res s' <- readChan (wInbox myCh)
          writeIORef sRef s'
          writeChan acks Ack
    PhaseB ->
      if pos == UL || pos == UR
        then do
          s <- readIORef sRef
          let pCh = chOf (south h w (i,j))
          writeChan (nInbox pCh) (Req s)
          Res sP <- readChan (sInbox myCh)
          let (s', sP') = rulesInteract rules (s, sP)
          writeIORef sRef s'
          writeChan (nInbox pCh) (Res sP')
          writeIORef posRef (margolusShift pos)
          writeChan acks Ack
        else do
          Req _ <- readChan (nInbox myCh)
          s <- readIORef sRef
          writeChan (sInbox (chOf (north h w (i,j)))) (Res s)
          Res s' <- readChan (nInbox myCh)
          writeIORef sRef s'
          writeIORef posRef (margolusShift pos)
          writeChan acks Ack

serverLoop :: AutoState s o -> Chan (AutomatonMsg o) -> IO ()
serverLoop st inbox = forever $ do
  msg <- receive inbox
  case msg of
    MsgStep -> do
      let h = aH st; w = aW st
      t <- readIORef (aT st)
      if odd h || odd w
        then do
          doMargolusStepIO (aRules st) h w (aStates st) t
          atomicModifyIORef' (aT st) (\x -> (x+1, ()))
        else do
          broadcast (StepMsg t PhaseA) (aSupIn st)
          waitNAcks (aAcks st) (h*w)
          broadcast (StepMsg t PhaseB) (aSupIn st)
          waitNAcks (aAcks st) (h*w)
          atomicModifyIORef' (aT st) (\x -> (x+1, ()))
    MsgObserve rc -> do
      rows <- mapM (mapM readIORef) (aStates st)
      reply rc (map (map (rulesObserve (aRules st))) rows)

doMargolusStepIO :: Rules s o -> Int -> Int -> [[IORef s]] -> Int -> IO ()
doMargolusStepIO rules h w srefs t = do
  let pos0 = buildGrid h w (\i j -> margolusInitial (i,j))
      posT = iterate (stepPosOnly h w) pos0 !! t
  void (stepStatesFromPos h w rules srefs posT)


stepPosOnly :: Int -> Int -> [[MargolusPos]] -> [[MargolusPos]]
stepPosOnly h w pos =
  map (map margolusShift) (sweepPosReverse h w pos (h-1) (w-1))

sweepPosReverse :: Int -> Int -> [[MargolusPos]] -> Int -> Int -> [[MargolusPos]]
sweepPosReverse h w pos i j
  | i < 0     = pos
  | j < 0     = sweepPosReverse h w pos (i-1) (w-1)
  | otherwise = case index2D pos i j of
      UL -> sweepPosReverse h w (setBlockPos h w pos i j) i (j-1)
      _  -> sweepPosReverse h w pos                     i (j-1)

setBlockPos :: Int -> Int -> [[MargolusPos]] -> Int -> Int -> [[MargolusPos]]
setBlockPos h w pos i j =
  let iE  = i;     jE  = (j+1) `mod` w
      iS  = (i+1) `mod` h; jS  = j
      iSE = iS;    jSE = jE
  in set2D (set2D (set2D (set2D pos i j   UL) iE  jE  UR) iS  jS  LL) iSE jSE LR


stepStatesFromPos
  :: Int -> Int -> Rules s o -> [[IORef s]] -> [[MargolusPos]] -> IO [[MargolusPos]]
stepStatesFromPos h w rules srefs pos =
  sweepStatesReverse h w rules srefs pos (h-1) (w-1)

sweepStatesReverse
  :: Int -> Int -> Rules s o -> [[IORef s]] -> [[MargolusPos]] -> Int -> Int -> IO [[MargolusPos]]
sweepStatesReverse h w rules srefs pos i j
  | i < 0     = pure pos
  | j < 0     = sweepStatesReverse h w rules srefs pos (i-1) (w-1)
  | otherwise = case index2D pos i j of
      UL -> processBlockIO h w rules srefs pos i j >>= \pos' ->
            sweepStatesReverse h w rules srefs pos' i (j-1)
      _  -> sweepStatesReverse h w rules srefs pos  i (j-1)

processBlockIO
  :: Int -> Int -> Rules s o -> [[IORef s]] -> [[MargolusPos]] -> Int -> Int -> IO [[MargolusPos]]
processBlockIO h w rules srefs pos i j = do
  let iE  = i;     jE  = (j+1) `mod` w
      iS  = (i+1) `mod` h; jS  = j
      iSE = iS;    jSE = jE
      r i' j' = srefs !! i' !! j'
  ul <- readIORef (r i   j  )
  ur <- readIORef (r iE  jE )
  ll <- readIORef (r iS  jS )
  lr <- readIORef (r iSE jSE)
  let (ul1, ur1) = rulesInteract rules (ul, ur)
      (ll1, lr1) = rulesInteract rules (ll, lr)
      (ul2, ll2) = rulesInteract rules (ul1, ll1)
      (ur2, lr2) = rulesInteract rules (ur1, lr1)
  writeIORef (r i   j  ) ul2
  writeIORef (r iE  jE ) ur2
  writeIORef (r iS  jS ) ll2
  writeIORef (r iSE jSE) lr2
  pure (setBlockPos h w pos i j)


broadcast :: SupMsg -> [[Chan SupMsg]] -> IO ()
broadcast m = mapM_ (mapM_ (`writeChan` m))

waitNAcks :: Chan Ack -> Int -> IO ()
waitNAcks c n = replicateM_ n (readChan c)

grid :: Int -> Int -> (Coord -> IO a) -> IO [[a]]
grid h w f = mapM (\i -> mapM (\j -> f (i,j)) [0..w-1]) [0..h-1]

at :: [[a]] -> Coord -> IO a
at m (i,j) = pure ((m !! i) !! j)

buildGrid :: Int -> Int -> (Int -> Int -> a) -> [[a]]
buildGrid h w f = [ [ f i j | j <- [0..w-1] ] | i <- [0..h-1] ]

index2D :: [[a]] -> Int -> Int -> a
index2D g i j = (g !! i) !! j

set2D :: [[a]] -> Int -> Int -> a -> [[a]]
set2D g i j x =
  let (rb, r:ra) = splitAt i g
      (cb, _:ca) = splitAt j r
  in  rb ++ [cb ++ (x:ca)] ++ ra

void :: IO a -> IO ()
void m = m >> pure ()
