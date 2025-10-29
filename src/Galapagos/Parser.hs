module Galapagos.Parser (parseGalapagos) where

import APL.AST
import APL.Parser (pExp)
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.Void (Void)
import Galapagos.Rules (Config(..), Params(..), Colour(..), defaultParams, Name)
import Text.Megaparsec
  ( Parsec, parse, errorBundlePretty, (<|>), try
  , many, some, satisfy, chunk, notFollowedBy, eof
  )
import Text.Megaparsec.Char (spaceChar)

type Parser = Parsec Void String

sc :: Parser ()
sc = many spaceChar >> pure ()

kw :: String -> Parser ()
kw w = do _ <- chunk w
          notFollowedBy (satisfy (\c -> isAlphaNum c || c=='_'||c=='-'||c=='.'))
          sc

sym :: String -> Parser ()
sym s = chunk s >> sc

pInt :: Parser Int
pInt = read <$> (some (satisfy isDigit) <* sc)

pName :: Parser String
pName = some (satisfy (\c -> isAlphaNum c || c=='_' || c=='-' || c=='.')) <* sc

hexVal :: Char -> Int
hexVal c | '0'<=c && c<='9' = fromEnum c - fromEnum '0'
         | 'a'<=lc && lc<='f' = 10 + fromEnum lc - fromEnum 'a'
         | otherwise = 0
  where lc = toLower c
        toLower x | 'A'<=x && x<='Z' = toEnum (fromEnum x + 32)
                  | otherwise        = x

twoHex :: Char -> Char -> Int
twoHex a b = 16*hexVal a + hexVal b

pColour :: Parser Colour
pColour = do
  _ <- chunk "#"
  [a,b,c,d,e,f] <- mapM (\_ -> satisfy (\ch -> isDigit ch || ('a'<=toLower ch && toLower ch<='f')))
                         [(),(),(),(),(),()]
  sc
  pure (RGB (twoHex a b) (twoHex c d) (twoHex e f))

pSize :: Parser (Int,Int)
pSize = try (do kw "height"; sym "="; h <- pInt; kw "width"; sym "="; w <- pInt; pure (h,w))
    <|>       (do kw "width";  sym "="; w <- pInt; kw "height"; sym "="; h <- pInt; pure (h,w))

pParam :: Parser (Params -> Params)
pParam =
      (kw "startHP"               >> sym "=" >> (\n p -> p{startHP=n}) <$> pInt)
  <|> (kw "invReproductionChance" >> sym "=" >> (\n p -> p{invReproductionChance=n}) <$> pInt)
  <|> (kw "maxHP"                 >> sym "=" >> (\n p -> p{maxHP=n}) <$> pInt)
  <|> (kw "lifespan"              >> sym "=" >> (\n p -> p{lifespan=n}) <$> pInt)
  <|> (kw "invDensity"            >> sym "=" >> (\n p -> p{invDensity=n}) <$> pInt)
  <|> (kw "payoffs"               >> sym "=" >>
       do sym "("; a<-pInt; sym ","; b<-pInt; sym ","; c<-pInt; sym ","; d<-pInt; sym ")"
          pure (\p -> p{payoffs=(a,b,c,d)}))

pParams :: Parser Params
pParams = do
  updaters <- many pParam
  pure (applyAll updaters defaultParams)
  where
    applyAll []     p = p
    applyAll (f:fs) p = applyAll fs (f p)


pStrategy :: Parser (Name, Colour, Exp)
pStrategy = do
  kw "strategy"
  n <- pName
  col <- pColour
  sym "{"
  e <- pExp
  sym "}"
  pure (n, col, e)

pStrategies :: Parser [(Name, Colour, Exp)]
pStrategies = many (try pStrategy)

pConfig :: Parser Config
pConfig = do
  sc
  (h,w) <- pSize
  ps    <- pParams
  ss    <- pStrategies
  eof
  pure (Config { cfgHeight=h, cfgWidth=w, cfgParams=ps, cfgStrategies=ss })

parseGalapagos :: FilePath -> String -> Either String Config
parseGalapagos fname s =
  case parse pConfig fname s of
    Left e  -> Left (errorBundlePretty e)
    Right x -> Right x
