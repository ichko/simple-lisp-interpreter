module Parser where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
  )
import Data.Char (isAlphaNum)
import ParserUtils
import qualified Specification as S
import Prelude hiding (span)

inBrackets :: Parser a -> Parser a
inBrackets p = char '(' *> ws *> p <* ws <* char ')'

identifier :: Parser S.Identifier
identifier = atLeast $ \c -> isAlphaNum c || c `elem` "+-*/><='"

atom :: Parser S.Atom
atom =
  orChain
    <|> S.IntValue <$> integer
    <|> S.StringValue <$> stringLiteral

expression :: Parser S.Expression
expression =
  orChain
    <|> S.Atom <$> atom
    <|> S.Identifier <$> identifier
    <|> S.Application <$> inBrackets (separated someWS expression)

program :: Parser S.Program
program = ws *> (S.Program <$> some (expression <* ws))

instance Parseable S.Program where
  parser = program

parseFile :: FilePath -> IO (ParserResult S.Program)
parseFile path = do
  code <- readFile path
  let parsed = runParser program code
  return parsed

main' :: IO ()
main' = do
  print "aaa"
  print (parse "(define a 5)" :: S.Program)
