module Specification where

type Identifier = String

type Error = String

newtype Program
  = Program [Expression]
  deriving (Show)

data Expression
  = Atom Atom
  | Identifier Identifier
  | Application [Expression]
  deriving (Show)

data Atom
  = IntValue Integer
  | StringValue String
  | Null
  deriving (Show, Eq)
