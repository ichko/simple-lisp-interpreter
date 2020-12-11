{-# LANGUAGE DeriveAnyClass #-}

module Specification where

class (Show a) => ShowCtor a where
  showCtor :: a -> String
  showCtor = head . words . show

class (ShowCtor a) => EqCtor a where
  (@==) :: a -> a -> Bool
  a @== b = showCtor a == showCtor b

  (@/=) :: a -> a -> Bool
  a @/= b = not (a @== b)

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

type Context = [(Identifier, Expression)]

type Error = String

type ValueFunction =
  Context -> [Expression] -> Either Error Value

data Value
  = IntValue Integer
  | BoolValue Bool
  | StringValue String
  | ListValue [Expression]
  | FunctionValue Identifier ValueFunction
  deriving (ShowCtor, EqCtor)

instance Show Value where
  show (IntValue val) = "IntValue " ++ show val
  show (BoolValue val) = "BoolValue " ++ show val
  show (StringValue val) = "StringValue " ++ show val
  show (ListValue val) = "ListValue " ++ show val
  show (FunctionValue name _func) = "FunctionValue " ++ name

-- int :: Value
-- int = IntValue 0

-- bool :: Value
-- bool = IntValue 0

-- string :: Value
-- string = IntValue 0

-- list :: Value
-- list = IntValue 0

-- func :: Value
-- func = FunctionValue "\\_" (\[] -> Right . Constant $ int)

-- typeAssert :: (String, [Value]) -> ValueFunction -> ValueFunction
-- typeAssert (msg, assertions) decorated arguments
--   | length assertions == length arguments =
--     Left $ "incorrect number of arguments (" ++ msg ++ ")"
--   | not typesEq =
--     Left $
--       "incorrect types. Found "
--         ++ show arguments
--         ++ ", should be "
--         ++ show assertions
--         ++ " ("
--         ++ msg
--         ++ ")"
--   | otherwise = decorated arguments
--   where
--     typesEq = all (uncurry (@==)) $ zip assertions arguments

-- (==>) :: (String, [Value]) -> ValueFunction -> ValueFunction
-- (==>) = typeAssert
