module Interpreter where

import Parser
import ParserUtils as P
import Specification

type Ctx = [(Identifier, HostVal)]

data HostVal
  = HostAtom Atom
  | HostFunc (Ctx -> [Expression] -> Either Error HostVal)

instance Eq HostVal where
  (HostAtom a) == (HostAtom b) = a == b
  _ == _ = False

instance Show HostVal where
  show (HostAtom a) = "HostAtom " ++ show a
  show (HostFunc f) = "HostFunc (...)"

eval :: Ctx -> Expression -> Either Error HostVal
eval _ (Atom a) = Right (HostAtom a)
eval ctx (Identifier id) =
  case lookup id ctx of
    Nothing -> Left $ "identifier missing from context '" ++ id ++ "'"
    Just hostVal -> Right hostVal
eval ctx (Application (funcExpr : args)) = do
  hostVal <- eval ctx funcExpr
  case hostVal of
    HostAtom _ -> Left "calling non callable"
    HostFunc f -> f ctx args

define :: Ctx -> [Expression] -> Either Error HostVal
define _ (Application (Identifier _ : argNames) : body) =
  Right . HostFunc $ \ctx argsExpr -> do
    args <- mapM (eval ctx) argsExpr
    let ids = map (\(Identifier id) -> id) argNames
        extendedCtx = zip ids args
    loop (extendedCtx ++ ctx) body
  where
    loop :: Ctx -> [Expression] -> Either Error HostVal
    loop _ [] = Left "empty body function"
    loop ctx [expr] = eval ctx expr
    loop ctx (h : t) = do
      case h of
        def@(Application (_ : (Application (Identifier id : _)) : _)) -> do
          hostVal <- eval ctx def
          loop ((id, hostVal) : ctx) t
        _ -> Left $ "error in function definition " ++ show h
define _ expr = Left $ "error in define call" ++ show expr

std :: [(Identifier, HostVal)]
std =
  [ ("define", HostFunc define),
    ( "if",
      HostFunc $ \ctx [boolExpr, thenExpr, elseExpr] -> do
        testVal <- eval ctx boolExpr
        let (HostAtom (IntValue test)) = testVal
        if test == 1
          then eval ctx thenExpr
          else eval ctx elseExpr
    ),
    ( "+",
      HostFunc $ \ctx [aExpr, bExpr] -> do
        aVal <- eval ctx aExpr
        bVal <- eval ctx bExpr
        let (HostAtom (IntValue a)) = aVal
            (HostAtom (IntValue b)) = bVal
        return . HostAtom . IntValue $ a + b
    ),
    ( "*",
      HostFunc $ \ctx [aExpr, bExpr] -> do
        aVal <- eval ctx aExpr
        bVal <- eval ctx bExpr
        let (HostAtom (IntValue a)) = aVal
            (HostAtom (IntValue b)) = bVal
        return . HostAtom . IntValue $ a * b
    ),
    ( "-",
      HostFunc $ \ctx [aExpr, bExpr] -> do
        aVal <- eval ctx aExpr
        bVal <- eval ctx bExpr
        let (HostAtom (IntValue a)) = aVal
            (HostAtom (IntValue b)) = bVal
        return . HostAtom . IntValue $ a - b
    ),
    ( "<=",
      HostFunc $ \ctx [aExpr, bExpr] -> do
        aVal <- eval ctx aExpr
        bVal <- eval ctx bExpr
        let (HostAtom (IntValue a)) = aVal
            (HostAtom (IntValue b)) = bVal
        return . HostAtom . IntValue $ if a <= b then 1 else 0
    ),
    ( "==",
      HostFunc $ \ctx [aExpr, bExpr] -> do
        aVal <- eval ctx aExpr
        bVal <- eval ctx bExpr
        let (HostAtom (IntValue a)) = aVal
            (HostAtom (IntValue b)) = bVal
        return . HostAtom . IntValue $ if a == b then 1 else 0
    )
  ]

evalProgram :: Program -> Either Error HostVal
evalProgram (Program program) = eval std rootExpr
  where
    rootExpr =
      Application $
        [ Identifier "define",
          Application [Identifier "#"]
        ]
          ++ program
          ++ [Application [Identifier "main"]]

interpret :: String -> Either Error HostVal
interpret src = do
  hostF <- evalProgram (P.parse src)
  let (HostFunc f) = hostF
  f std []

interpretFile :: FilePath -> IO (HostVal)
interpretFile path = do
  code <- readFile path
  let (Right hostVal) = interpret code
  return hostVal

main'' :: IO ()
main'' = do
  -- print $ interpret "(define a 2)"
  -- print $ interpret "(define a (+ 2 2)) (define a (+ 2 3))"
  print $ runParser program "(define (a) 1)"
  print $ interpret "(define (inc a) (+ a 1)) (define (main) (inc 4))"
  let srcCode =
        "                            \
        \ (define (fact n)           \
        \    (if (== n 0)            \
        \       1                    \
        \       (* n (fact (- n 1))) \
        \    )                       \
        \ )                          \
        \ (define (main)             \
        \    (+ 1 (+ 1 3))           \
        \ )                          \
        \                            "
  print $ interpret srcCode
