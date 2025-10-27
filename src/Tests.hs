-- | Omnibus test suite grouped by which task each test applies to.
module Tests (tests) where

import APL.AST
import APL.Eval
import APL.Monad
import APL.Parser
import BlockAutomaton.Rules
import BlockAutomaton.Server
import qualified BlockAutomaton.Simulation as Simulation
import Control.Monad
import Galapagos.Parser
import qualified Galapagos.Rules as Galapagos
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

galapagosParserTest :: String -> Galapagos.Config -> TestTree
galapagosParserTest s e =
  testCase s $
    case parseGalapagos "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

galapagosParserTestFail :: String -> TestTree
galapagosParserTestFail s =
  testCase s $
    case parseGalapagos "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

-- Definitions of finches corresponding to the strategies in the assignment
-- text. These may be useful for testing, but you do not have to use them.
invalid, cheater, samaritan, flipflop :: Galapagos.Finch
invalid =
  Galapagos.Finch
    { Galapagos.finchID = 0,
      Galapagos.finchHP = 10,
      Galapagos.finchRoundsLeft = 3,
      Galapagos.finchColour = Galapagos.RGB 0xff 0xff 0xff,
      Galapagos.finchStrategy =
        void $ eval mempty $ Galapagos.finchStrategyExp invalid,
      Galapagos.finchStrategyExp =
        CstBool False
    }
cheater =
  Galapagos.Finch
    { Galapagos.finchID = 0,
      Galapagos.finchHP = 10,
      Galapagos.finchRoundsLeft = 3,
      Galapagos.finchColour = Galapagos.RGB 0x00 0x00 0xff,
      Galapagos.finchStrategy =
        void $ eval mempty $ Galapagos.finchStrategyExp cheater,
      Galapagos.finchStrategyExp =
        ForLoop
          ("x", CstInt 0)
          ("i", CstInt 100)
          (Let "unused" Meet Ignore)
    }
samaritan =
  Galapagos.Finch
    { Galapagos.finchID = 0,
      Galapagos.finchHP = 20,
      Galapagos.finchRoundsLeft = 4,
      Galapagos.finchColour = Galapagos.RGB 0xff 0x00 0x00,
      Galapagos.finchStrategy =
        void $ eval mempty $ Galapagos.finchStrategyExp samaritan,
      Galapagos.finchStrategyExp =
        ForLoop
          ("x", CstInt 0)
          ("i", CstInt 100)
          (Let "unused" Meet Groom)
    }
flipflop =
  Galapagos.Finch
    { Galapagos.finchID = 0,
      Galapagos.finchHP = 20,
      Galapagos.finchRoundsLeft = 4,
      Galapagos.finchColour = Galapagos.RGB 0x00 0xff 0x00,
      Galapagos.finchStrategy =
        void $ eval mempty $ Galapagos.finchStrategyExp flipflop,
      Galapagos.finchStrategyExp =
        ForLoop
          ("x", CstInt 0)
          ("i", CstInt 100)
          (Let "unused" Meet (If (Eql (Mul (Div (Var "i") (CstInt 2)) (CstInt 2)) (Var "i")) Ignore Groom))
    }

defaultConfig :: Galapagos.Config
defaultConfig =
  Galapagos.Config
    { Galapagos.cfgHeight = 10,
      Galapagos.cfgWidth = 10,
      Galapagos.cfgParams = Galapagos.defaultParams,
      Galapagos.cfgStrategies =
        [ ( "samaritan",
            Galapagos.finchColour samaritan,
            Galapagos.finchStrategyExp samaritan
          ),
          ( "cheater",
            Galapagos.finchColour cheater,
            Galapagos.finchStrategyExp cheater
          ),
          ( "flipflop",
            Galapagos.finchColour flipflop,
            Galapagos.finchStrategyExp flipflop
          )
        ]
    }

-- This function runs a block automaton rule set for some number of iterations
-- using both the simulator and the server, then checks if they produce the same
-- result.
testEquivalence :: (Eq obs, Show obs) => Rules state obs -> Int -> IO ()
testEquivalence rules steps = do
  let h = 8
      w = 8
      simulated =
        Simulation.observeGrid rules $
          (!! steps) $
            iterate (Simulation.stepGrid rules) $
              Simulation.initialGrid rules h w
  s <- startAutomaton rules h w
  replicateM_ steps $ stepAutomaton s
  real <- observeAutomaton s
  real @?= simulated

taskAtests :: TestTree
taskAtests =
  testGroup
    "Task A"
    [
      testCase "Set literal: {}" $
        case eval mempty (Set []) of
          Pure (ValSet vs) -> vs @?= []
          Pure v           -> assertFailure $ "Expected ValSet [], got: " ++ show v
          _                -> assertFailure "Expected pure value for empty set"

    , testCase "Set literal: {1, 2}" $
        case eval mempty (Set [CstInt 1, CstInt 2]) of
          Pure (ValSet vs) -> vs @?= [ValInt 1, ValInt 2]
          Pure v           -> assertFailure $ "Expected ValSet [1,2], got: " ++ show v
          _                -> assertFailure "Expected pure value for set literal"

    , testCase "Union: {1,2} union {3}" $
        let e = Union (Set [CstInt 1, CstInt 2]) (Set [CstInt 3])
        in case eval mempty e of
             Pure (ValSet vs) -> vs @?= [ValInt 1, ValInt 2, ValInt 3]
             Pure v           -> assertFailure $ "Expected ValSet [1,2,3], got: " ++ show v
             _                -> assertFailure "Expected pure value for union"

    , testCase "Union type error: left not a set" $
        let e = Union (CstInt 1) (Set [CstInt 2])
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure for union with non-set left operand"
             _      -> pure ()

    , testCase "Union type error: right not a set" $
        let e = Union (Set [CstInt 1]) (CstBool True)
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure for union with non-set right operand"
             _      -> pure ()

    , testCase "Without: {1,2,3} without {2,4} = {1,3}" $
        let e = Without (Set [CstInt 1, CstInt 2, CstInt 3]) (Set [CstInt 2, CstInt 4])
        in case eval mempty e of
             Pure (ValSet vs) -> vs @?= [ValInt 1, ValInt 3]
             Pure v           -> assertFailure $ "Expected ValSet [1,3], got: " ++ show v
             _                -> assertFailure "Expected pure value for without"

    , testCase "Without type error: left not a set" $
        let e = Without (CstInt 1) (Set [])
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure for without with non-set left operand"
             _      -> pure ()

    , testCase "Without type error: right not a set" $
        let e = Without (Set []) (CstInt 1)
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure for without with non-set right operand"
             _      -> pure ()

    , testCase "Elem: 2 elem {1,2,3} = true" $
        let e = Elem (CstInt 2) (Set [CstInt 1, CstInt 2, CstInt 3])
        in case eval mempty e of
             Pure (ValBool b) -> b @?= True
             Pure v           -> assertFailure $ "Expected ValBool True, got: " ++ show v
             _                -> assertFailure "Expected pure value for elem (true case)"

    , testCase "Elem: 4 elem {1,2,3} = false" $
        let e = Elem (CstInt 4) (Set [CstInt 1, CstInt 2, CstInt 3])
        in case eval mempty e of
             Pure (ValBool b) -> b @?= False
             Pure v           -> assertFailure $ "Expected ValBool False, got: " ++ show v
             _                -> assertFailure "Expected pure value for elem (false case)"

    , testCase "Elem type error: RHS not a set" $
        let e = Elem (CstInt 1) (CstInt 2)
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure for elem with non-set RHS"
             _      -> pure ()

    , testCase "Nested sets: { {1}, {2} } union { {3} }" $
        let s12 = Set [Set [CstInt 1], Set [CstInt 2]]
            s3  = Set [Set [CstInt 3]]
            e   = Union s12 s3
        in case eval mempty e of
             Pure (ValSet vs) ->
               vs @?=
                 [ ValSet [ValInt 1]
                 , ValSet [ValInt 2]
                 , ValSet [ValInt 3]
                 ]
             Pure v -> assertFailure $ "Expected set-of-sets, got: " ++ show v
             _      -> assertFailure "Expected pure value for union of nested sets"

    , testCase "Elem with bools: true elem {false, true}" $
        let e = Elem (CstBool True) (Set [CstBool False, CstBool True])
        in case eval mempty e of
             Pure (ValBool b) -> b @?= True
             Pure v           -> assertFailure $ "Expected ValBool True, got: " ++ show v
             _                -> assertFailure "Expected pure value for elem with bools"

    , testCase "Set element evaluation fails on error (first element fails)" $
        let bad = Div (CstInt 1) (CstInt 0)
            e   = Set [bad, CstInt 2]
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure constructing set when an element fails"
             _      -> pure ()

    , testCase "Set element evaluation fails on error (later element fails)" $
        let bad = Div (CstInt 1) (CstInt 0)
            e   = Set [CstInt 1, bad]
        in case eval mempty e of
             Pure _ -> assertFailure "Expected failure constructing set when an element fails"
             _      -> pure ()
    ]

taskBtests :: TestTree
taskBtests =
  testGroup
    "Task B"
    [
      parserTest "{}" (Set [])
    , parserTest "{1}" (Set [CstInt 1])
    , parserTest "{1, 2, 3}" (Set [CstInt 1, CstInt 2, CstInt 3])

    , parserTest "a elem {1}"
        (Elem (Var "a") (Set [CstInt 1]))
    , parserTest "a elem S elem T"
        (Elem (Elem (Var "a") (Var "S")) (Var "T"))

    , parserTest "a union b union c"
        (Union (Var "a") (Union (Var "b") (Var "c")))
    , parserTest "S without T without U"
        (Without (Var "S") (Without (Var "T") (Var "U")))


    , parserTest "x elem {1} union {2}"
        (Elem (Var "x") (Union (Set [CstInt 1]) (Set [CstInt 2])))
      , parserTest "a elem S without T union U"
        (Elem (Var "a") (Without (Var "S") (Union (Var "T") (Var "U"))))

    , parserTest "1 + 2 elem {3}"
      (Elem (Add (CstInt 1) (CstInt 2)) (Set [CstInt 3]))
    , parserTest "(a union b) elem S"
        (Elem (Union (Var "a") (Var "b")) (Var "S"))

    , parserTest "let u = meet in groom"
        (Let "u" Meet Groom)

    , parserTestFail "{1,}"
    , parserTestFail "{ , 1 }"
    , parserTestFail "elem {1} 2"
    ]

taskCtests :: TestTree
taskCtests =
  testGroup
    "Task C"
    [ testCase "Smoothen(0)" $ testEquivalence smoothen 0,
      testCase "Smoothen(1)" $ testEquivalence smoothen 1,
      testCase "Smoothen(2)" $ testEquivalence smoothen 2
    ]

taskDtests :: TestTree
taskDtests =
  testGroup
    "Task D"
    []

taskEtests :: TestTree
taskEtests =
  testGroup
    "Task E"
    []

taskFtests :: TestTree
taskFtests =
  testGroup
    "Task F"
    []

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ taskAtests,
      taskBtests,
      taskCtests,
      taskDtests,
      taskEtests,
      taskFtests
    ]
