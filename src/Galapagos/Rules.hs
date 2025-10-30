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

--True will mean groom, False will mean ignore
rollDice :: Int -> StdGen -> (Bool, StdGen)
rollDice sides gen =
  let (result, newGen) = uniformR (1, sides) gen
   in (result == 1, newGen)


createFinch :: Params -> Int -> Colour -> Exp -> Finch
createFinch params fid colour stratExp =
  Finch
    { finchID = fid,
      finchHP = startHP params,
      finchRoundsLeft = lifespan params,
      finchColour = colour,
      finchStrategy = createStrategy stratExp,
      finchStrategyExp = stratExp
    }

createStrategy :: Exp -> Strategy
createStrategy exp = fmap (const ()) (eval envEmpty exp)

-- The first 'Int' is the 's' parameter for seeding the RNG.
initial :: Int -> Config -> (Int, Int) -> CellState
-- Given a seed, configuration, and cell coordinates, produces the initial state of the cell.
-- Should be using randomness to determine if a finch is present based on the density parameter,
initial seed cfg (i,j) =
  let g0         = mkStdGen (seed + i*10000 + j)
      ps         = cfgParams cfg
      strats     = cfgStrategies cfg
      n          = length strats
  in case n of
       0 -> CellState { cellFinch = Nothing, cellRNG = g0 }
       _ -> let (ok,g1) = rollDice (invDensity ps) g0
            in if not ok
                 then CellState { cellFinch = Nothing, cellRNG = g1 }
                 else let k        = (i + j) `mod` n
                          (_,col,e) = strats !! k
                          (fid,g2) = uniform g1
                      in CellState { cellFinch = Just (createFinch ps fid col e)
                                   , cellRNG   = g2 }

reproduce :: Params -> (CellState, CellState) -> (CellState, CellState)
-- """
-- A finch should reproduce if it rolls a 1 on a dice with 
-- sides equal to the inverse reproduction chance parameter.
-- """
reproduce params (a,b) =
  case (cellFinch a, cellFinch b) of
    (Just f, Nothing) ->
      let (ok,g') = rollDice (invReproductionChance params) (cellRNG b)
      in if not ok then (a, b { cellRNG = g' })
         else let (fid,g'') = uniform g'
                  child = createFinch params fid (finchColour f) (finchStrategyExp f)
              in (a, b { cellRNG = g'', cellFinch = Just child })
    (Nothing, Just f) ->
      let (b',a') = reproduce params (b,a) in (a',b')
    _ -> (a,b)

meet :: Params -> (Finch, Finch) -> (Maybe Finch, Maybe Finch)
-- """
-- A pair of finches meet and decide whether to groom or ignore each other.
-- Returns the updated finches after applying the payoffs based on their decisions.
-- """
meet ps (a, b) =
  case (step a b, step b a) of
    (Right (ag, ka), Right (bg, kb)) ->
      let (da, db) = payoff ps ag bg
          cap x    = min (maxHP ps) x
          a' = a { finchHP = cap (finchHP a + da)
                 , finchStrategy = ka bg }
          b' = b { finchHP = cap (finchHP b + db)
                 , finchStrategy = kb ag }
       in (Just a', Just b')
    (Left (), Right _)  -> (Nothing, Just b)
    (Right _, Left ())  -> (Just a, Nothing)
    (Left (), Left ())  -> (Nothing, Nothing)
  where
    step self other =
      case finchStrategy self of
        Free (MeetOp k) ->
          case k (finchID other) of
            Free (GroomOp k2)  -> Right (True,  k2)
            Free (IgnoreOp k2) -> Right (False, k2)
            _                  -> Left ()
        _ -> Left ()

    

groom :: Params -> (CellState, CellState) -> (CellState, CellState)
-- """
-- Two finches in neighboring cells interact by grooming each other.
-- This function should update their states based on the grooming interaction.
-- """
groom params (a, b)= 
  case (cellFinch a, cellFinch b) of
    (Just fa, Just fb) ->
      case meet params (fa,fb) of
        (Nothing,Nothing) -> (a{cellFinch=Nothing}, b{cellFinch=Nothing})
        (Nothing, Just fb')-> (a{cellFinch=Nothing}, b{cellFinch=Just fb'})
        (Just fa', Nothing)-> (a{cellFinch=Just fa'}, b{cellFinch=Nothing})
        (Just fa', Just fb')->(a{cellFinch=Just fa'}, b{cellFinch=Just fb'})
    _ -> (a,b) 

-- """
-- A finch ages by decrementing its rounds left.
-- If the rounds left reaches zero, the finch dies.
-- """
age :: CellState -> CellState
age cell = 
  case cellFinch cell of
    Just finch -> cell { cellFinch = Just (finch { finchRoundsLeft = finchRoundsLeft finch - 1 }) }
    Nothing    -> cell

-- """
-- A finch dies, removing it from the cell.
-- """
kill :: CellState -> CellState
kill c = 
  case cellFinch c of
    Just f | finchHP f <= 0 || finchRoundsLeft f <= 0 -> c { cellFinch = Nothing }
    _ -> c


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
