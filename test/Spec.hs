import Interpreter
import Parser
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  program <- parseFile "test/example-3.scm"
  pPrint program
  int <- interpretFile "test/example.scm"
  print int
