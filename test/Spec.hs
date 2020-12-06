import Parser

main :: IO ()
main = do
  program <- parseFile "test/example.scm"
  print program
