module Parser where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
  )
import Data.Char (isAlphaNum)
import ParserUtils
  ( Parseable (..),
    Parser (runParser),
    ParserResult,
    atLeast,
    bool,
    char,
    defaultP,
    integer,
    list,
    orChain,
    parse,
    separated,
    someWS,
    string,
    stringLiteral,
    ws,
  )
import qualified Specification as S
import Prelude hiding (span)

inBrackets :: Parser a -> Parser a
inBrackets p = char '(' *> ws *> p <* ws <* char ')'

inDef :: Parser a -> Parser a
inDef p = inBrackets (string "define" *> ws *> p)

identifier :: Parser S.Identifier
identifier = atLeast $ \c -> isAlphaNum c || c `elem` "+-*/><='"

value :: Parser S.Value
value =
  orChain
    <|> S.IntValue <$> integer
    <|> S.BoolValue <$> bool
    <|> S.StringValue <$> stringLiteral
    <|> S.ListValue <$> list expression

expression :: Parser S.Expression
expression =
  orChain
    <|> S.Atom <$> value
    <|> S.Reference <$> identifier
    <|> variable
    <|> function
    <|> application

args :: Parser [S.Identifier]
args = ws *> inBrackets (separated someWS identifier) <* ws

variable :: Parser S.Expression
variable = inDef $ S.Variable <$> (identifier <* someWS) <*> expression

functionId :: Parser [Char]
functionId = defaultP "\\" identifier

function :: Parser S.Expression
function = inDef $ S.Function <$> functionId <*> args <*> some (expression <* ws)

application :: Parser S.Expression
application =
  inBrackets $
    S.Application <$> identifier <*> many (someWS *> expression)

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
  print (parse "(define a 5)" :: S.Program)
  print $ runParser program "(define a t)"
  print $ runParser program "(define a \"ala bala\")"
  print $ runParser program "(define a [\"ala bala\", 1, True])"
  print $ runParser program "(define t [1, (define () (a))])"
  print $ runParser program "(define a (def b 1))"
  print $ runParser program "(define a (sum))"
  print $ runParser program "(define a (sum 1))"
  print $ runParser program "(define a (sum 1 ab (+ 1 1)))"
  print $ runParser program "(define () (a))"
  print $ runParser program "(define (ab cd) 1)"
  print $ runParser program "(define (ab cd) (c d))"
  print $ runParser program "(define sum (ab c) a)"

  program <- parseFile "example.scm"
  print program
