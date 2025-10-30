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
import System.Random (mkStdGen)

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

runSequential :: Rules s o -> Int -> Int -> Int -> [[o]]
runSequential rules h w steps =
  let g0 = Simulation.initialGrid rules h w
      gN = iterate (Simulation.stepGrid rules) g0 !! steps
  in  Simulation.observeGrid rules gN

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
    [
      testCase "Smoothen: steps=0, 4x4"  $ testEquivalenceHW smoothen 4 4 0,
      testCase "Smoothen: steps=1, 4x4"  $ testEquivalenceHW smoothen 4 4 1,
      testCase "Smoothen: steps=2, 4x4"  $ testEquivalenceHW smoothen 4 4 2,
      testCase "Smoothen: steps=5, 4x4"  $ testEquivalenceHW smoothen 4 4 5,
      testCase "Smoothen: steps=10, 4x4" $ testEquivalenceHW smoothen 4 4 10,
      testCase "Smoothen: steps=7, 4x6 rectangular" $
        testEquivalenceHW smoothen 4 6 7
    ]
  where
    testEquivalenceHW :: (Eq obs, Show obs) => Rules state obs -> Int -> Int -> Int -> IO ()
    testEquivalenceHW rules h w steps = do
      let simulated = runSequential rules h w steps
      s <- startAutomaton rules h w
      replicateM_ steps $ stepAutomaton s
      real <- observeAutomaton s
      real @?= simulated


taskDtests :: TestTree
taskDtests =
  testGroup
    "Task D"
    [
      testCase "meet: samaritan vs cheater => both legal (both Just)" $ do
        let params = Galapagos.defaultParams
        let (ra, rb) = Galapagos.meet params (samaritan, cheater)
        case (ra, rb) of
          (Just _, Just _) -> pure ()
          other            -> assertFailure $ "Expected (Just,Just), got: " ++ showPair other

    , testCase "meet: cheater vs samaritan => both legal (both Just)" $ do
        let params = Galapagos.defaultParams
        let (ra, rb) = Galapagos.meet params (cheater, samaritan)
        case (ra, rb) of
          (Just _, Just _) -> pure ()
          other            -> assertFailure $ "Expected (Just,Just), got: " ++ showPair other

    , testCase "meet: invalid (illegal) vs samaritan => (Nothing, Just originalSamaritan)" $ do
        let params = Galapagos.defaultParams
            s0     = samaritan
            (ra, rb) = Galapagos.meet params (invalid, s0)
        case (ra, rb) of
          (Nothing, Just s1) -> do
            Galapagos.finchHP s1          @?= Galapagos.finchHP s0
            Galapagos.finchRoundsLeft s1  @?= Galapagos.finchRoundsLeft s0
            Galapagos.finchColour s1      @?= Galapagos.finchColour s0
          other -> assertFailure $ "Expected (Nothing, Just...), got: " ++ showPair other

    , testCase "meet: samaritan vs invalid (illegal) => (Just originalSamaritan, Nothing)" $ do
        let params = Galapagos.defaultParams
            s0     = samaritan
            (ra, rb) = Galapagos.meet params (s0, invalid)
        case (ra, rb) of
          (Just s1, Nothing) -> do
            Galapagos.finchHP s1          @?= Galapagos.finchHP s0
            Galapagos.finchRoundsLeft s1  @?= Galapagos.finchRoundsLeft s0
            Galapagos.finchColour s1      @?= Galapagos.finchColour s0
          other -> assertFailure $ "Expected (Just..., Nothing), got: " ++ showPair other
    , testCase "meet: invalid vs invalid => (Nothing, Nothing)" $ do
        let params = Galapagos.defaultParams
            (ra, rb) = Galapagos.meet params (invalid, invalid)
        case (ra, rb) of
          (Nothing, Nothing) -> pure ()
          other              -> assertFailure $ "Expected (Nothing,Nothing), got: " ++ showPair other
    , testCase "meet: samaritan vs samaritan => both Just and HPs not unchanged simultaneously" $ do
        let params = Galapagos.defaultParams
            a0 = samaritan; b0 = samaritan { Galapagos.finchID = 1 }
            (ra, rb) = Galapagos.meet params (a0, b0)
        case (ra, rb) of
          (Just a1, Just b1) -> do
            let sameA = Galapagos.finchHP a1 == Galapagos.finchHP a0
                sameB = Galapagos.finchHP b1 == Galapagos.finchHP b0
            assertBool "Expected some HP change on GG" (not (sameA && sameB))
          other -> assertFailure $ "Expected (Just,Just), got: " ++ showPair other

    , testCase "meet: cheater vs cheater => both Just and at least one HP changed" $ do
        let params = Galapagos.defaultParams
            a0 = cheater; b0 = cheater { Galapagos.finchID = 2 }
            (ra, rb) = Galapagos.meet params (a0, b0)
        case (ra, rb) of
          (Just a1, Just b1) -> do
            let sameA = Galapagos.finchHP a1 == Galapagos.finchHP a0
                sameB = Galapagos.finchHP b1 == Galapagos.finchHP b0
            assertBool "Expected some HP change on II" (not (sameA && sameB))
          other -> assertFailure $ "Expected (Just,Just), got: " ++ showPair other

    , testCase "meet: samaritan vs cheater => asymmetric HP effects (qualitative)" $ do
        let params = Galapagos.defaultParams
            a0 = samaritan; b0 = cheater
            (ra, rb) = Galapagos.meet params (a0, b0)
        case (ra, rb) of
          (Just a1, Just b1) -> do
            let da = Galapagos.finchHP a1 - Galapagos.finchHP a0
                db = Galapagos.finchHP b1 - Galapagos.finchHP b0
            assertBool "Expected asymmetric HP deltas for GI" (da /= db)
          other -> assertFailure $ "Expected (Just,Just), got: " ++ showPair other

    , testCase "strategy progression: flipflop vs cheater twice changes flipflop HP between meets" $ do
        let params = Galapagos.defaultParams
            f0 = flipflop; c0 = cheater { Galapagos.finchID = 42 }
            (rf1, rc1) = Galapagos.meet params (f0, c0)
        case (rf1, rc1) of
          (Just f1, Just c1) -> do
            let (rf2, rc2) = Galapagos.meet params (f1, c1)
            case (rf2, rc2) of
              (Just f2, Just _) ->
                assertBool "flipflop HP should differ between meet1 and meet2"
                  (Galapagos.finchHP f1 /= Galapagos.finchHP f2)
              other -> assertFailure $ "2nd meet expected (Just,Just), got: " ++ showPair other
          other -> assertFailure $ "1st meet expected (Just,Just), got: " ++ showPair other
    ]
  where
    showPair (ma, mb) = "(" ++ maybe "Nothing" (const "Just _") ma
                         ++ "," ++ maybe "Nothing" (const "Just _") mb ++ ")"


taskEtests :: TestTree
taskEtests =
  testGroup
    "Task E (Config parser with new grammar)"
    [
      let h = 6; w = 8
          cfg = defaultConfig { Galapagos.cfgHeight = h
                              , Galapagos.cfgWidth  = w
                              , Galapagos.cfgParams = Galapagos.defaultParams
                              , Galapagos.cfgStrategies = []
                              }
          src = "height = 6 width = 8\n"
      in galapagosParserTest src cfg,
      let h = 12; w = 10
          cfg = defaultConfig { Galapagos.cfgHeight = h
                              , Galapagos.cfgWidth  = w
                              , Galapagos.cfgParams = Galapagos.defaultParams
                              , Galapagos.cfgStrategies = []
                              }
          src = "width = 10 height = 12\n"
      in galapagosParserTest src cfg,
      let p  = Galapagos.defaultParams { Galapagos.startHP = 37 }
          cfg = defaultConfig { Galapagos.cfgHeight = 10
                              , Galapagos.cfgWidth  = 10
                              , Galapagos.cfgParams = p
                              , Galapagos.cfgStrategies = []
                              }
          src = unlines
            [ "height = 10 width = 10"
            , "startHP = 37"
            ]
      in galapagosParserTest src cfg,
      let p = Galapagos.defaultParams
               { Galapagos.startHP               = 15
               , Galapagos.invReproductionChance = 7
               , Galapagos.maxHP                 = 50
               , Galapagos.lifespan              = 99
               , Galapagos.invDensity            = 4
               , Galapagos.payoffs               = (1,2,3,4)
               }
          cfg = defaultConfig { Galapagos.cfgHeight = 14
                              , Galapagos.cfgWidth  = 16
                              , Galapagos.cfgParams = p
                              , Galapagos.cfgStrategies = []
                              }
          src = unlines
            [ "height = 14 width = 16"
            , "startHP = 15"
            , "invReproductionChance = 7"
            , "maxHP = 50"
            , "lifespan = 99"
            , "invDensity = 4"
            , "payoffs = (1, 2, 3, 4)"
            ]
      in galapagosParserTest src cfg,

      let p = Galapagos.defaultParams { Galapagos.startHP = 9 }
          cfg = defaultConfig { Galapagos.cfgHeight = 8
                              , Galapagos.cfgWidth  = 8
                              , Galapagos.cfgParams = p
                              , Galapagos.cfgStrategies = []
                              }
          src = unlines
            [ "height = 8 width = 8"
            , "startHP = 3"
            , "startHP = 9"
            ]
      in galapagosParserTest src cfg,

      let cfg = defaultConfig { Galapagos.cfgHeight = 10
                              , Galapagos.cfgWidth  = 10
                              , Galapagos.cfgParams = Galapagos.defaultParams
                              , Galapagos.cfgStrategies =
                                  [ ( "samaritan"
                                    , Galapagos.finchColour samaritan
                                    , Galapagos.finchStrategyExp samaritan
                                    )
                                  ]
                              }
          src = unlines
            [ "height = 10 width = 10"
            , "strategy samaritan #ff0000 {"
            , "  loop x = 0 for i < 100 do let unused = meet in groom"
            , "}"
            ]
      in galapagosParserTest src cfg,

      let strat1 = ( "flip-flop.v2"
                   , Galapagos.RGB 0x12 0x34 0x56
                   , Galapagos.finchStrategyExp flipflop
                   )
          strat2 = ( "cheater_beta"
                   , Galapagos.RGB 0xAA 0xBB 0xCC
                   , Galapagos.finchStrategyExp cheater
                   )
          cfg = defaultConfig { Galapagos.cfgHeight = 6
                              , Galapagos.cfgWidth  = 6
                              , Galapagos.cfgParams = Galapagos.defaultParams
                              , Galapagos.cfgStrategies = [strat1, strat2]
                              }
          src = unlines
            [ "width = 6 height = 6"
            , "strategy flip-flop.v2 #123456 {"
            , "  loop x = 0 for i < 100 do"
            , "  let unused = meet in"
            , "  if i / 2 * 2 == i then ignore else groom"
            , "}"
            , "strategy cheater_beta #AaBbCc {"
            , "  loop x = 0 for i < 100 do let unused = meet in ignore"
            , "}"
            ]
      in galapagosParserTest src cfg,

      let p = Galapagos.defaultParams { Galapagos.invDensity = 11 }
          cfg = defaultConfig { Galapagos.cfgHeight = 4
                              , Galapagos.cfgWidth  = 4
                              , Galapagos.cfgParams = p
                              , Galapagos.cfgStrategies =
                                  [ ( "cheater"
                                    , Galapagos.finchColour cheater
                                    , Galapagos.finchStrategyExp cheater
                                    )
                                  , ( "samaritan"
                                    , Galapagos.finchColour samaritan
                                    , Galapagos.finchStrategyExp samaritan
                                    )
                                  ]
                              }
          src = unlines
            [ "height = 4 width = 4"
            , "invDensity = 11"
            , "strategy cheater #0000ff { loop x = 0 for i < 100 do let unused = meet in ignore }"
            , "strategy samaritan #ff0000 { loop x = 0 for i < 100 do let unused = meet in groom }"
            ]
      in galapagosParserTest src cfg,

      let p = Galapagos.defaultParams { Galapagos.payoffs = (10,20,30,40) }
          cfg = defaultConfig { Galapagos.cfgHeight = 2
                              , Galapagos.cfgWidth  = 2
                              , Galapagos.cfgParams = p
                              , Galapagos.cfgStrategies = []
                              }
          src = "height = 2 width = 2\npayoffs = (10,20,30,40)\n"
      in galapagosParserTest src cfg,
      galapagosParserTestFail "height 10 width = 8",

      galapagosParserTestFail (unlines
        [ "height = 6 width = 6"
        , "hpStart = 10"
        ]),

      galapagosParserTestFail (unlines
        [ "width = 8 height = 8"
        , "payoffs = (1,2,3)"
        ]),

      galapagosParserTestFail (unlines
        [ "height = 6 width = 6"
        , "strategy badcolor #12zz9a { true }"
        ]),

      galapagosParserTestFail (unlines
        [ "height = 4 width = 4"
        , "strategy x #000000 true"
        ]),

      galapagosParserTestFail "height = 5",

      galapagosParserTestFail (unlines
        [ "height = 2 width = 2"
        , "payoffs = (1,,2,3,4)"
        ]),
      galapagosParserTestFail (unlines
        [ "height = 6 width = 6"
        , "strategy bad name #000000 { true }"
        ])
    ]



taskFtests :: TestTree
taskFtests =
  testGroup
    "Task F (initial / reproduce / groom / age / kill + E2E parity)"
    [

      -- F0. Initial strategy tiling reflected in colour pattern at step 0 (density=1).
      testCase "Initial: (i+j) mod n tiling -> colour pattern at step 0" $ do
        let h = 6; w = 8
            cfg = defaultConfig
                    { Galapagos.cfgHeight = h
                    , Galapagos.cfgWidth  = w
                    , Galapagos.cfgParams = (Galapagos.cfgParams defaultConfig)
                        { Galapagos.invDensity = 1 } -- fully occupied
                    }
            rules = Galapagos.galapagos 0 cfg
            obs0  = Simulation.observeGrid rules (Simulation.initialGrid rules h w)
            cols  = map (\(_,c,_) -> c) (Galapagos.cfgStrategies cfg)
            expected = [ [ cols !! ((i+j) `mod` length cols) | j <- [0..w-1] ]
                       | i <- [0..h-1] ]
        obs0 @?= expected

    , testCase "Reproduce: empty neighbor + invReproductionChance=1 -> child with startHP & lifespan" $ do
        let p   = (Galapagos.cfgParams defaultConfig)
                    { Galapagos.startHP = 17
                    , Galapagos.lifespan = 9
                    , Galapagos.invReproductionChance = 1
                    }
            gPar   = mkStdGen 999
            gEmpty = mkStdGen 111
            left0  = cellWith samaritan gPar
            right0 = emptyCell gEmpty
            (left1, right1) = Galapagos.reproduce p (left0, right0)
        case (getFinch right1) of
          Nothing -> assertFailure "Expected child in right cell"
          Just c  -> do
            Galapagos.finchHP c         @?= Galapagos.startHP p
            Galapagos.finchRoundsLeft c @?= Galapagos.lifespan p
        -- parent remains
        case getFinch left1 of
          Nothing  -> assertFailure "Expected parent to remain in left cell"
          Just p1  -> do
            Galapagos.finchHP p1           @?= Galapagos.finchHP samaritan
            Galapagos.finchRoundsLeft p1   @?= Galapagos.finchRoundsLeft samaritan
            Galapagos.finchColour p1       @?= Galapagos.finchColour samaritan
            Galapagos.finchStrategyExp p1  @?= Galapagos.finchStrategyExp samaritan

    , testCase "Reproduce: both occupied -> no reproduction" $ do
        let p = (Galapagos.cfgParams defaultConfig) { Galapagos.invReproductionChance = 1 }
            g1 = mkStdGen 1; g2 = mkStdGen 2
            l0 = cellWith samaritan g1
            r0 = cellWith cheater   g2
            (l1, r1) = Galapagos.reproduce p (l0, r0)
        fmap Galapagos.finchID (getFinch l1) @?= Just (Galapagos.finchID samaritan)
        fmap Galapagos.finchID (getFinch r1) @?= Just (Galapagos.finchID cheater)

    , testCase "Groom: illegal finch is removed, legal meeting obeys maxHP cap" $ do
        let baseP = Galapagos.cfgParams defaultConfig
            gL = mkStdGen 7; gR = mkStdGen 8
            -- illegal vs legal
            (l1, r1) = Galapagos.groom baseP (cellWith invalid gL, cellWith samaritan gR)
        case getFinch l1 of
          Nothing -> pure ()
          Just _  -> assertFailure "Expected left cell to be empty (illegal finch removed)"

        case getFinch r1 of
          Nothing -> assertFailure "Legal finch should remain when other is illegal"
          Just _  -> pure ()
        -- legal GG with cap
        let p' = baseP { Galapagos.maxHP = 40, Galapagos.payoffs = (100, -2, 3, -4) }
            a0 = samaritan { Galapagos.finchHP = 39 }
            b0 = samaritan { Galapagos.finchHP = 39, Galapagos.finchID = 1 }
            (_, r2) = Galapagos.groom p' (cellWith a0 gL, cellWith b0 gR)
        case getFinch r2 of
          Nothing -> assertFailure "Finch should remain after legal GG"
          Just fr2 -> assertBool "HP must not exceed maxHP" (Galapagos.finchHP fr2 <= Galapagos.maxHP p')

    , testCase "Age: finch roundsLeft decremented; empty stays empty" $ do
        let gL = mkStdGen 10; gR = mkStdGen 11
            f  = samaritan { Galapagos.finchRoundsLeft = 3 }
            l1 = Galapagos.age (cellWith f gL)
            r1 = Galapagos.age (emptyCell gR)
        case getFinch l1 of
          Just f1 -> Galapagos.finchRoundsLeft f1 @?= 2
          _       -> assertFailure "Expected finch after age"
        case getFinch r1 of
          Nothing -> pure ()
          Just _  -> assertFailure "Expected right cell to remain empty after age"



    , testCase "Kill: removes when HP<=0 or roundsLeft<=0" $ do
      let g = mkStdGen 12
          k1l = Galapagos.kill (cellWith (samaritan { Galapagos.finchHP = 0 }) g)
          k2l = Galapagos.kill (cellWith (samaritan { Galapagos.finchRoundsLeft = 0 }) g)
      case getFinch k1l of
        Nothing -> pure ()
        Just _  -> assertFailure "Expected kill to remove finch when HP<=0"

      case getFinch k2l of
        Nothing -> pure ()
        Just _  -> assertFailure "Expected kill to remove finch when roundsLeft<=0"



    , testCase "Payoff mapping (1,-2,3,-4): GG, II, and I¬G deltas applied via groom" $ do
        let p = Galapagos.cfgParams defaultConfig -- payoffs = (1,-2,3,-4)
            g1 = mkStdGen 21; g2 = mkStdGen 22
            -- GG: Samaritan vs Samaritan
            a0 = samaritan { Galapagos.finchHP = 10, Galapagos.finchID = 100 }
            b0 = samaritan { Galapagos.finchHP = 20, Galapagos.finchID = 101 }
            (l1, r1) = Galapagos.groom p (cellWith a0 g1, cellWith b0 g2)
        case (getFinch l1, getFinch r1) of
          (Just a1, Just b1) -> do
            (Galapagos.finchHP a1 - Galapagos.finchHP a0) @?= 1
            (Galapagos.finchHP b1 - Galapagos.finchHP b0) @?= 1
          _ -> assertFailure "Expected both finches after GG"
        -- II: Cheater vs Cheater
        let c0 = cheater { Galapagos.finchHP = 7, Galapagos.finchID = 200 }
            d0 = cheater { Galapagos.finchHP = 6, Galapagos.finchID = 201 }
            (l2, r2) = Galapagos.groom p (cellWith c0 g1, cellWith d0 g2)
        case (getFinch l2, getFinch r2) of
          (Just c1, Just d1) -> do
            (Galapagos.finchHP c1 - Galapagos.finchHP c0) @?= (-2)
            (Galapagos.finchHP d1 - Galapagos.finchHP d0) @?= (-2)
          _ -> assertFailure "Expected both finches after II"
        -- I¬G: cheater ignores, samaritan grooms
        let e0 = cheater   { Galapagos.finchHP = 10, Galapagos.finchID = 300 }
            f0 = samaritan { Galapagos.finchHP = 10, Galapagos.finchID = 301 }
            (l3, r3) = Galapagos.groom p (cellWith e0 g1, cellWith f0 g2)
        case (getFinch l3, getFinch r3) of
          (Just e1, Just f1) -> do
            (Galapagos.finchHP e1 - Galapagos.finchHP e0) @?= 3   -- c
            (Galapagos.finchHP f1 - Galapagos.finchHP f0) @?= (-4) -- d
          _ -> assertFailure "Expected both finches after I¬G"

      -- End-to-end parity vs sequential (server must match simulator)
    , testCase "Parity: dense (invDensity=1), steps={0,1,5,10}" $ do
        let cfg = defaultConfig { Galapagos.cfgHeight = 4
                                , Galapagos.cfgWidth  = 4
                                , Galapagos.cfgParams = (Galapagos.cfgParams defaultConfig)
                                    { Galapagos.invDensity = 1 }
                                }
        mapM_ (runBothAndCompare cfg) [0,1,5,10]

    , testCase "Parity: sparse + always reproduce (invDensity=3, invReproductionChance=1), steps=6" $ do
        let cfg = defaultConfig { Galapagos.cfgHeight = 10
                                , Galapagos.cfgWidth  = 10
                                , Galapagos.cfgParams = (Galapagos.cfgParams defaultConfig)
                                    { Galapagos.invDensity = 3
                                    , Galapagos.invReproductionChance = 1
                                    }
                                , Galapagos.cfgStrategies =
                                    [ ( "samaritan"
                                      , Galapagos.finchColour samaritan
                                      , Galapagos.finchStrategyExp samaritan
                                      )
                                    , ( "cheater"
                                      , Galapagos.finchColour cheater
                                      , Galapagos.finchStrategyExp cheater
                                      )
                                    ]
                                }
        runBothAndCompare cfg 6

    , testCase "Parity: never reproduce (invReproductionChance very large), steps=8" $ do
        let cfg = defaultConfig { Galapagos.cfgHeight = 8
                                , Galapagos.cfgWidth  = 6
                                , Galapagos.cfgParams = (Galapagos.cfgParams defaultConfig)
                                    { Galapagos.invDensity = 2
                                    , Galapagos.invReproductionChance = 999999
                                    }
                                , Galapagos.cfgStrategies =
                                    [ ( "samaritan"
                                      , Galapagos.finchColour samaritan
                                      , Galapagos.finchStrategyExp samaritan
                                      )
                                    , ( "cheater"
                                      , Galapagos.finchColour cheater
                                      , Galapagos.finchStrategyExp cheater
                                      )
                                    ]
                                }
        runBothAndCompare cfg 8
    ]
  where
    -- Local tiny utilities (kept local to avoid adding global helpers)
    getFinch cs = Galapagos.cellFinch cs
    cellWith f g = Galapagos.CellState { Galapagos.cellFinch = Just f, Galapagos.cellRNG = g }
    emptyCell g  = Galapagos.CellState { Galapagos.cellFinch = Nothing, Galapagos.cellRNG = g }

    runBothAndCompare cfg steps = do
      let h     = Galapagos.cfgHeight cfg
          w     = Galapagos.cfgWidth  cfg
          rules = Galapagos.galapagos 0 cfg
          sim0  = Simulation.initialGrid rules h w
          simN  = iterate (Simulation.stepGrid rules) sim0 !! steps
          simObs = Simulation.observeGrid rules simN
      srv <- startAutomaton rules h w
      replicateM_ steps $ stepAutomaton srv
      srvObs <- observeAutomaton srv
      srvObs @?= simObs


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
