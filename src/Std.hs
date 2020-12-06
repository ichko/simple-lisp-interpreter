module Std where

import Specification

type ErrorMessage = String

newtype Error = Error ErrorMessage

assertAllTypes :: ErrorMessage -> [Value] -> (a -> Value) -> Either ErrorMessage ()
assertAllTypes _ [] _ = Right ()
assertAllTypes msg (h : t) ctor = undefined

type StdFunction = [Value] -> Either Error Value

binaryFunction ::
  (Value -> a) ->
  (b -> Value) ->
  (a -> a -> b) ->
  StdFunction
binaryFunction unpack pack func [a, b] =
  Right $ pack (unpack a `func` unpack b)

binInt :: (Integer -> Integer -> Integer) -> StdFunction
binInt = binaryFunction (\(IntValue i) -> i) IntValue

binBool :: (Bool -> Bool -> Bool) -> StdFunction
binBool = binaryFunction (\(BoolValue i) -> i) BoolValue

binIntBool :: (Integer -> Integer -> Bool) -> StdFunction
binIntBool = binaryFunction (\(IntValue i) -> i) BoolValue

ifFunc :: StdFunction
ifFunc [BoolValue condition, thenVal, elseVal] =
  Right $ if condition then thenVal else elseVal

printFunc :: StdFunction
printFunc [] = Right $ StringValue "\n"
printFunc [IntValue h] = Right . StringValue $ show h ++ "\n"
printFunc [BoolValue h] = Right . StringValue $ show h ++ "\n"
printFunc [StringValue h] = Right . StringValue $ h ++ "\n"
printFunc [ListValue h] = Right . StringValue $ "[" ++ (let Right (StringValue a) = mapM printFunc h in a) ++ "\n"

std :: [(Identifier, StdFunction)]
std =
  [ ("+", binInt (+)),
    ("-", binInt (-)),
    ("*", binInt (*)),
    ("/", binInt div),
    --
    (">", binIntBool (>)),
    ("<", binIntBool (<)),
    (">=", binIntBool (>=)),
    ("<=", binIntBool (<=)),
    ("==", binIntBool (==)),
    --
    ("&&", binBool (&&)),
    ("||", binBool (||)),
    --
    ("if", ifFunc),
    ("print", printFunc)
  ]
