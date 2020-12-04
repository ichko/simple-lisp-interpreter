module SLispParser where

import Control.Applicative
import Data.Char
import Parser
import Prelude hiding (span)

type Identifier = String

type Program = [Expression]

data Expression
  = Constant Value
  | Reference Identifier
  | Application Identifier [Expression]
  | Variable Identifier Expression
  | Function Identifier [Identifier] [Expression]
  deriving (Show)

data Value
  = IntValue Integer
  | BoolValue Bool
  deriving (Show)

inBrackets :: Parser a -> Parser a
inBrackets p = char '(' *> ws *> p <* ws <* char ')'

inDef :: Parser a -> Parser a
inDef p = inBrackets (string "define" *> ws *> p)

identifier :: Parser Identifier
identifier = atLeast $ \c -> isAlphaNum c || c `elem` "+-*/><='"

value :: Parser Value
value = IntValue <$> integer <|> BoolValue <$> bool

expression :: Parser Expression
expression =
  orChain
    <|> (Constant <$> value)
    <|> (Reference <$> identifier)
    <|> variable
    <|> function
    <|> application

args :: Parser [Identifier]
args =
  ws *> inBrackets (separated someWS identifier) <* ws

variable :: Parser Expression
variable = inDef $ Variable <$> (identifier <* someWS) <*> expression

functionId :: Parser [Char]
functionId = defaultP "" identifier

function :: Parser Expression
function = inDef $ Function <$> functionId <*> args <*> some expression

application :: Parser Expression
application =
  inBrackets $
    Application <$> identifier <*> many (someWS *> expression)

program :: Parser Program
program = ws *> some (expression <* ws)

parseFile :: FilePath -> IO (ParserResult Program)
parseFile path = do
  code <- readFile path
  let parsed = runParser program code
  return parsed

main :: IO ()
main = do
  print $ runParser program "(def a 5)"
  print $ runParser program "(def a t)"
  print $ runParser program "(def a (def b 1))"
  print $ runParser program "(def a (sum))"
  print $ runParser program "(def a (sum 1))"
  print $ runParser program "(def a (sum 1 ab (+ 1 1)))"

  print $ runParser program "(def () (a))"
  print $ runParser program "(def (ab cd) 1)"

  print $ runParser program "(def (ab cd) (c d))"

  print $ runParser program "(def sum (ab c) a)"

  program <- parseFile "example.scm"
  print program
