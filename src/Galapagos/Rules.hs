module Galapagos.Rules
  ( galapagos,
    Colour (..),
    Name,
    Strategy,
    Strategies,
    Config (..),
    Params (..),
    defaultParams,

    -- * For testing
    HP,
    Rounds,
    Finch (..),
    CellState (..),
    initial,
    reproduce,
    groom,
    age,
    kill,
    meet,
  )
where

import APL.AST (Exp)
import APL.Eval
import APL.Monad (EvalM, EvalOp (..), FinchID, Free (..))
import BlockAutomaton.Rules
import System.Random (StdGen, mkStdGen, uniform, uniformR)
import Prelude hiding (interact)

type HP = Int

type Rounds = Int

type Strategy = EvalM ()

type Name = String

-- | The colour represented as the red, green, and blue channels.
data Colour = RGB Int Int Int deriving (Show, Eq)

type Strategies = [(Name, Colour, Exp)]

-- Parameters for the world simulation.
data Params = Params
  { -- The starting hit points of a new finch.
    startHP :: HP,
    -- The inverse of the reproduction chance, i.e., to reproduce you must roll
    -- a 1 on a dice with this many sides.
    invReproductionChance :: Int,
    -- The maximum health points a finch can accumulate.
    maxHP :: HP,
    -- The lifespan of a new finch.
    lifespan :: Rounds,
    -- The inverse of the chance for a cell to be initially populated, i.e., a cell is populated if it rolls a 1 on a dice with this many sides.
    invDensity :: Int,
    -- The three payoff parameters:
    --   a. the payoff of both finches grooming.
    --   b. the payoff of both finches ignoring.
    --   c. the payoff of being groomed but ignoring.
    --   d. the payoff of being ignored but grooming.
    payoffs :: (Int, Int, Int, Int)
  }
  deriving (Eq, Show)

defaultParams :: Params
defaultParams =
  Params
    { startHP = 14,
      invReproductionChance = 6,
      maxHP = 40,
      lifespan = 23,
      invDensity = 4,
      payoffs = (1, -2, 3, -4)
    }

-- Helper function for computing the payoff based on groom/ignore decisions
-- (represneted as True/False respectively).
payoff :: Params -> Bool -> Bool -> (Int, Int)
payoff (Params {payoffs = (a, b, c, d)}) = f
  where
    f True True = (a, a)
    f False False = (b, b)
    f True False = (d, c)
    f False True = (c, d)

data Finch = Finch
  { finchID :: Int,
    finchHP :: HP,
    finchRoundsLeft :: Rounds,
    -- The colour is used for visualisation, but has no semantic significance.
    finchColour :: Colour,
    -- The current strategy.
    finchStrategy :: Strategy,
    -- The expression that is evaluated to produce the strategy.
    finchStrategyExp :: Exp
  }

-- A cell contains a random number generator and may also contain a finch.
data CellState = CellState
  { cellFinch :: Maybe Finch,
    cellRNG :: StdGen
  }

-- The first 'Int' is the 's' parameter for seeding the RNG.
initial :: Int -> Config -> (Int, Int) -> CellState
-- Given a seed, configuration, and cell coordinates, produces the initial state of the cell.
-- Should be using randomness to determine if a finch is present based on the density parameter,

initial = undefined

reproduce :: Params -> (CellState, CellState) -> (CellState, CellState)
-- """
-- A finch should reproduce if it rolls a 1 on a dice with 
-- sides equal to the inverse reproduction chance parameter.
-- """
reproduce = undefined

meet :: Params -> (Finch, Finch) -> (Maybe Finch, Maybe Finch)
-- """
-- A pair of finches meet and decide whether to groom or ignore each other.
-- Returns the updated finches after applying the payoffs based on their decisions.
-- """
meet = undefined

groom :: Params -> (CellState, CellState) -> (CellState, CellState)
-- """
-- Two finches in neighboring cells interact by grooming each other.
-- This function should update their states based on the grooming interaction.
-- """
groom = undefined

-- """
-- A finch ages by decrementing its rounds left.
-- If the rounds left reaches zero, the finch dies.
-- """
age :: CellState -> CellState
age = undefined

-- """
-- A finch dies, removing it from the cell.
-- """
kill :: CellState -> CellState
kill = undefined


both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

interact :: Params -> (CellState, CellState) -> (CellState, CellState)
interact params =
  both kill
    . both age
    . groom params
    . reproduce params

observe :: CellState -> Colour
observe cell = case cellFinch cell of
  Nothing -> black
  Just finch -> finchColour finch
  where
    black = RGB 0 0 0

data Config = Config
  { cfgHeight :: Int,
    cfgWidth :: Int,
    cfgParams :: Params,
    cfgStrategies :: [(Name, Colour, Exp)]
  }
  deriving (Eq, Show)

galapagos :: Int -> Config -> Rules CellState Colour
galapagos seed cfg =
  Rules
    { rulesInitial = initial seed cfg,
      rulesInteract = interact $ cfgParams cfg,
      rulesObserve = observe
    }
