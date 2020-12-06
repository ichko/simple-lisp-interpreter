module Specification where

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
  | StringValue String
  | ListValue [Expression]
  deriving (Show)
