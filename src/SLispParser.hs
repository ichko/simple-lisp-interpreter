module SLispParser where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
  )
import Data.Char (isAlphaNum)
import Parser
import Prelude hiding (span)

type Identifier = String

newtype Program
  = Program [Expression]
  deriving (Show)

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
args = ws *> inBrackets (separated someWS identifier) <* ws

variable :: Parser Expression
variable = inDef $ Variable <$> (identifier <* someWS) <*> expression

functionId :: Parser [Char]
functionId = defaultP "\\" identifier

function :: Parser Expression
function = inDef $ Function <$> functionId <*> args <*> some expression

application :: Parser Expression
application =
  inBrackets $
    Application <$> identifier <*> many (someWS *> expression)

program :: Parser Program
program = ws *> (Program <$> some (expression <* ws))

instance Parseable Program where
  parser = program

parseFile :: FilePath -> IO (ParserResult Program)
parseFile path = do
  code <- readFile path
  let parsed = runParser program code
  return parsed

main' :: IO ()
main' = do
  print $ runParser program "(define a 5)"
  print $ runParser program "(define a t)"
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
