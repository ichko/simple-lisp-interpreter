module Interpreter where

import Parser (parseFile)
import ParserUtils as P
import Specification

data ContextVal
  = Native Expression
  | External [Identifier] (Environment -> Either Error Value)

getContextValArgs :: ContextVal -> [Identifier]
getContextValArgs (Native (Function _ args _)) = args
getContextValArgs (External args _) = args
getContextValArgs _ = []

type Environment = [(Identifier, ContextVal)]

evalContextVal :: ContextVal -> Environment -> Either Error Value
evalContextVal (Native expr) env = eval env expr
evalContextVal (External _ f) env = f env

evalFromEnv :: Identifier -> Environment -> Either Error Value
evalFromEnv identifier env =
  case lookup identifier env of
    Nothing -> Left $ "identifier not found " ++ identifier
    Just contextVal -> evalContextVal contextVal env

evalFunction :: Environment -> Expression -> Either Error Value
evalFunction env f@(Function name _args expressions) =
  iterate ((name, Native f) : env) expressions
  where
    iterate :: Environment -> [Expression] -> Either Error Value
    iterate _env [] = Left "function with no body called"
    iterate env [expr] = eval env expr
    iterate env (expr : t) =
      case expr of
        v@(Variable id _) -> iterate ((id, Native v) : env) t
        f@(Function id _ _) -> iterate ((id, Native f) : env) t
        _ -> iterate env t

eval :: Environment -> Expression -> Either Error Value
eval _ (Atom val) = Right val
eval env (Reference id) = evalFromEnv id env
eval env (Variable _id expression) = eval env expression
eval env (Application id args) = do
  case lookup id env of
    (Just contextVal) ->
      let paramsEnv = zip (getContextValArgs contextVal) (map Native args) ++ env
       in evalContextVal contextVal paramsEnv
    _ -> Left $ "trying to call undefined function " ++ id
eval ctx expr = evalFunction ctx expr

unaryFunc :: (Value -> a) -> (b -> Value) -> (a -> b) -> ContextVal
unaryFunc unpackA packB op = External ["a", "b"] $ \env -> do
  a <- evalFromEnv "a" env
  let _a = unpackA a
  return . packB $ op _a

binFunc :: (Value -> a) -> (Value -> b) -> (c -> Value) -> (a -> b -> c) -> ContextVal
binFunc unpackA unpackB packC op = External ["a", "b"] $ \env -> do
  a <- evalFromEnv "a" env
  b <- evalFromEnv "b" env
  let _a = unpackA a
      _b = unpackB b
  return . packC $ op _a _b

ifFunc :: ContextVal
ifFunc = External ["test", "if", "then"] $ \env -> do
  testExpr <- evalFromEnv "test" env
  let test = unpackBool testExpr
  if test
    then evalFromEnv "if" env
    else evalFromEnv "then" env

printFunc :: ContextVal
printFunc = External ["x"] $ \env -> do
  x <- evalFromEnv "x" env
  return . StringValue . show $ x

unpackInt :: Value -> Integer
unpackInt (IntValue a) = a

unpackBool :: Value -> Bool
unpackBool (BoolValue a) = a

stdEnv :: [(Identifier, ContextVal)]
stdEnv =
  [ ("+", binFunc unpackInt unpackInt IntValue (+)),
    ("-", binFunc unpackInt unpackInt IntValue (-)),
    ("*", binFunc unpackInt unpackInt IntValue (*)),
    ("/", binFunc unpackInt unpackInt IntValue div),
    --
    (">", binFunc unpackInt unpackInt BoolValue (>)),
    ("<", binFunc unpackInt unpackInt BoolValue (<)),
    (">=", binFunc unpackInt unpackInt BoolValue (>=)),
    ("<=", binFunc unpackInt unpackInt BoolValue (<=)),
    ("==", binFunc unpackInt unpackInt BoolValue (==)),
    ("!=", binFunc unpackInt unpackInt BoolValue (/=)),
    --
    ("&&", binFunc unpackBool unpackBool BoolValue (&&)),
    ("||", binFunc unpackBool unpackBool BoolValue (||)),
    ("~", unaryFunc unpackBool BoolValue not),
    --
    ("if", ifFunc),
    ("print", printFunc)
  ]

evalProgram :: Program -> Either Error Value
evalProgram (Program program) = eval stdEnv rootExpr
  where
    rootExpr = Function "#" [] program

interpret :: String -> Either Error Value
interpret src = evalProgram $ P.parse src

interpretFile :: FilePath -> IO ()
interpretFile path = do
  code <- readFile path
  let parsed = interpret code
  print parsed

main :: IO ()
main = do
  -- print $ interpret "(define a 2)"
  -- print $ interpret "(define a (+ 2 2)) (define a (+ 2 3))"
  print $ interpret "(define ++ (a) (+ 1 a)) (++ 4)"
