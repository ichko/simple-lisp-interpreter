module Interpreter where

import Parser ()
import qualified ParserUtils as P
import Specification

lookupCtx :: Context -> Identifier -> Either Error Expression
lookupCtx ctx identifier =
  case lookup identifier ctx of
    Nothing -> Left $ "identifier not found " ++ identifier
    Just expression -> Right expression

evalFunction :: Context -> Expression -> Either Error Value
evalFunction ctx f@(Function name _args expressions) =
  iterate referentialCtx expressions
  where
    referentialCtx = (name, f) : ctx

    iterate _ctx [] = Left "function with no body called"
    iterate ctx [expr] = eval ctx expr
    iterate ctx (expr : t) =
      case expr of
        v@(Variable id _) -> iterate ((id, v) : ctx) t
        f@(Function id _ _) -> iterate ((id, f) : ctx) t
        _ -> iterate ctx t

eval :: Context -> Expression -> Either Error Value
eval _ (Constant val) = Right val
eval ctx (Reference id) = do
  expression <- lookupCtx ctx id
  eval ctx expression
eval ctx (Variable _id expression) = eval ctx expression
eval ctx (Application id args) = do
  expression <- lookupCtx ctx id
  case expression of
    (Constant (FunctionValue _ f)) -> f ctx args
    f@(Function _ argNames _) ->
      let paramsCtx = zip argNames args ++ ctx in eval paramsCtx f
    _ -> Left $ "trying to call undefined function " ++ id
eval ctx expr = evalFunction ctx expr

binInt :: (Integer -> Integer -> Integer) -> ValueFunction
binInt op ctx params = do
  vals <- mapM (eval ctx) params
  let [IntValue a, IntValue b] = vals
  return (IntValue $ op a b)

binBool :: (Bool -> Bool -> Bool) -> ValueFunction
binBool op ctx params = do
  vals <- mapM (eval ctx) params
  let [BoolValue a, BoolValue b] = vals
  return (BoolValue $ op a b)

binIntBool :: (Integer -> Integer -> Bool) -> ValueFunction
binIntBool op ctx params = do
  vals <- mapM (eval ctx) params
  let [IntValue a, IntValue b] = vals
  return (BoolValue $ op a b)

ifFunc :: ValueFunction
ifFunc ctx [condition, first, second] = do
  wrappedTest <- eval ctx condition
  let (BoolValue test) = wrappedTest
  if test
    then eval ctx first
    else eval ctx second

printFunc :: ValueFunction
printFunc ctx params = do
  vals <- mapM (eval ctx) params
  return . StringValue . show $ vals

stdEnv :: [(Identifier, ValueFunction)]
stdEnv =
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
    ("!=", binIntBool (/=)),
    --
    ("&&", binBool (&&)),
    ("||", binBool (||)),
    --
    ("if", ifFunc),
    ("print", printFunc)
  ]

stdCtx :: [(Identifier, Expression)]
stdCtx =
  map
    ( \(id, func) ->
        (id, Constant . FunctionValue id $ func)
    )
    stdEnv

evalProgram :: Program -> Either Error Value
evalProgram (Program program) = eval stdCtx rootExpr
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
  print $ interpret "(define a 2)"
  print $ interpret "(define a (+ 2 2)) (define a (+ 2 3))"
