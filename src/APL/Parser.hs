module APL.Parser (parseAPL, pExp) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    sepBy,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "loop",
    "for",
    "do",
    "meet",
    "ignore",
    "groom",
    "without",
    "union",
    "elem"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      pSet,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")",
      lKeyword "groom" *> pure Groom,
      lKeyword "ignore" *> pure Ignore,
      lKeyword "meet" *> pure Meet
    ]

pSet :: Parser Exp
pSet = do
  lString "{"
  elems <- pExp `sepBy` lString ","
  lString "}"
  pure $ Set elems

pFExp :: Parser Exp
pFExp = chain =<< pAtom
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      try $
        ForLoop
          <$> (lKeyword "loop" *> ((,) <$> lVName <* lString "=" <*> pAtom))
          <*> (lKeyword "for" *> ((,) <$> lVName <* lString "<" <*> pExp))
          <*> (lKeyword "do" *> pExp),
      pFExp
    ]

pExp3 :: Parser Exp
pExp3 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "**"
            y <- pLExp
            Pow x <$> chain y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp3
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp3
            chain $ Div x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp2
            chain $ Add x y,
          do
            lString "-"
            y <- pExp2
            chain $ Sub x y,
          pure x
        ]
--union and without will be right associative
pUnionWithout :: Parser Exp
pUnionWithout = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            f <- (Union <$ lKeyword "union") <|> (Without <$ lKeyword "without")
            y <- pUnionWithout
            chain $ f x y
          ,pure x
        ]


pElem :: Parser Exp
pElem = pUnionWithout >>= chain
  where
    chain x =
      choice
        [ do
            lKeyword "elem"
            y <- pUnionWithout
            chain $ Elem x y,
          pure x
        ]

pEq :: Parser Exp
pEq = pElem >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pElem
            chain $ Eql x y,
          pure x
        ]


pExp :: Parser Exp
pExp = pEq

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
