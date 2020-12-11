module Specification where

type Identifier = String

type Error = String

newtype Program
  = Program [Expression]
  deriving (Show)

data Expression
  = Atom Value
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
