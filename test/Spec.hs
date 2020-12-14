import Parser
import Interpreter
import Specification

import Shower (shower)
import Test.Hspec

tests = hspec $ do
  describe "Interpreter.interpret" $ do
    it "parses int atom" $ do
      interpret "(define (main) 1)"
        `shouldBe` (Right . HostAtom . IntValue $ 1)
    it "parses string atom" $ do
      interpret "(define (main) \"1\")" 
        `shouldBe` (Right . HostAtom . StringValue $ "1")
    it "parses string atom" $ do
      interpret "(define (main) \"1\")"
        `shouldBe` (Right . HostAtom . StringValue $ "1")

  describe "Interpreter.interpretFile" $ do
    it "parses example-1" $ do
      val <- interpretFile "test/example-1.scm"
      val `shouldBe` (HostAtom . IntValue $ 6765)
    it "parses example-2" $ do
      val <- interpretFile "test/example-2.scm"
      val `shouldBe` (HostAtom . IntValue $ 6)
    it "parses example-3" $ do
      val <- interpretFile "test/example-3.scm"
      val `shouldBe` (HostAtom . IntValue $ 3628800)

main :: IO ()
main = do
  program <- parseFile "test/example-3.scm"
  -- putStrLn $ shower program
  tests
