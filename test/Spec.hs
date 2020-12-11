import Interpreter

main :: IO ()
main = do
  program <- interpretFile "test/example.scm"
  print program
