{-# LANGUAGE DataKinds #-}

module Parser (testFunc, parseExp) where

import Control.Monad
import Lambdas
import Test.QuickCheck
import Text.ParserCombinators.Parsec

lambdaSymbol :: Parser Char
lambdaSymbol = oneOf "λ\\"

parseAbs :: Parser LExp
parseAbs = do
  _ <- lambdaSymbol
  var <- many1 letter
  _ <- char '.'
  LAbs var <$> parseExp

parseVar :: Parser LExp
parseVar = do
  varname <- many1 (noneOf " .λ\\()")
  return $ LVar varname

parseApp :: Parser LExp
parseApp = do
  _ <- char '('
  exp1 <- parseExp
  _ <- char ' '
  exp2 <- parseExp
  _ <- char ')'
  return $ App exp1 exp2

parseExp :: Parser LExp
parseExp = parseIntChurch <|> parseVar <|> parseApp <|> parseAbs

parseIntChurch :: Parser LExp
parseIntChurch = liftM (churchEncoding . read) $ many1 digit

parseConst :: Parser LConst
parseConst = do
  _ <- string "let"
  skipMany1 spaces
  constname <- many1 letter
  skipMany1 spaces
  _ <- string ":= "
  constexp <- parseExp
  return (constname, constexp)

-- testing parser on reading church numerals
testParserChurchNum :: Int -> Property
testParserChurchNum n = parse parseExp "lambda" (show n) === Right (churchEncoding n)

testFunc :: IO ()
testFunc = do
  quickCheck (forAll (elements [0 .. 100]) testParserChurchNum)
  let teststring = "\\m.\\n.\\f.\\x.(m (f ((n f) x))) "
  case parse parseExp "lambda" teststring of
    Left err -> putStrLn "error"
    Right lambdaexp -> do
      print lambdaexp
      print $ betaReduce lambdaexp (churchEncoding 5)
