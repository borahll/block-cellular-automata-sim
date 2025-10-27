module BlockAutomaton.Server
  ( Automaton,
    startAutomaton,
    stepAutomaton,
    observeAutomaton,
  )
where

import BlockAutomaton.Rules
import Control.Concurrent
import Control.Monad (forM, forever, replicateM, when)
import GenServer

-- TODO
data AutomatonMsg obs

data Automaton obs = Automaton (Server (AutomatonMsg obs))

startAutomaton :: Rules state obs -> Int -> Int -> IO (Automaton obs)
startAutomaton rules height width = undefined

stepAutomaton :: Automaton obs -> IO ()
stepAutomaton = undefined

observeAutomaton :: Automaton obs -> IO [[obs]]
observeAutomaton = undefined
