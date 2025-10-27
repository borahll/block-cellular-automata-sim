module Galapagos.Parser (parseGalapagos) where

import APL.AST
import APL.Parser (pExp)
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.Void (Void)
import Galapagos.Rules
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- This is a dummy config returned so the visualisation can be run before the
-- parser is implemented. Delete this once you have implemented the parser.
dummyConfig :: Config
dummyConfig =
  Config
    { cfgHeight = 70,
      cfgWidth = 70,
      cfgParams = defaultParams,
      cfgStrategies =
        [ ( "samaritan",
            RGB 255 0 0,
            ForLoop ("x", CstInt 0) ("i", CstInt 100) (Let "unused" Meet Groom)
          ),
          ( "cheater",
            RGB 0 0 255,
            ForLoop ("x", CstInt 0) ("i", CstInt 100) (Let "unused" Meet Ignore)
          ),
          ( "flipflop",
            RGB 0 255 0,
            ForLoop ("x", CstInt 0) ("i", CstInt 100) (Let "unused" Meet (If (Eql (Mul (Div (Var "i") (CstInt 2)) (CstInt 2)) (Var "i")) Ignore Groom))
          )
        ]
    }

-- TODO: change this to be a proper parser instead of just returning the
-- dummyConfig. This definition is only present such that the visualisation
-- program will work (somewhat) before Task E has been implemented.
parseGalapagos :: FilePath -> String -> Either String Config
parseGalapagos _fname _s = Right dummyConfig
