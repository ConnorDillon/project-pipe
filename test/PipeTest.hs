module Main (main) where

import Pipe ( parser, AST(..) )
import Interpreter ( Value(..), eval )
import Data.Void ( Void )
import qualified Text.Megaparsec as MP
import Test.QuickCheck ( Testable(property) )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.Megaparsec ( shouldFailOn, shouldParse )

parse :: String -> Either (MP.ParseErrorBundle String Void) AST
parse = MP.parse parser ""

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "parses integers" $
      property $ \x -> parse (show x) `shouldParse` LitInt x
    it "parses floats" $
      property $ \x -> parse (show x) `shouldParse` LitFloat x
    it "parses strings" $
      property $ \x -> parse (show x) `shouldParse` LitString x
    it "parses booleans" $ do
      parse "true" `shouldParse` LitBool True
      parse "false" `shouldParse` LitBool False
    it "parses null" $
      parse "null" `shouldParse` LitNull
    it "parses symbols" $ do
      parse "nullFoo" `shouldParse` Symbol "nullFoo"
      parse "foo1" `shouldParse` Symbol "foo1"
      parse "foo_bar" `shouldParse` Symbol "foo_bar"
      parse "BAR" `shouldParse` Symbol "BAR"
    it "repects keywords" $
      parse `shouldFailOn` "then"
    it "parses if statements" $
      parse "if true then 1 else 2" `shouldParse` Expr
        [ Symbol "if"
        , LitBool True
        , LitInt 1
        , LitInt 2
        ]
    it "parses expressions" $
      parse "foo 1 2 + bar 3" `shouldParse` Expr
        [ Symbol "+"
        , Expr [Symbol "foo", LitInt 1, LitInt 2]
        , Expr [Symbol "bar", LitInt 3]
        ]
    it "parses lambdas" $
      parse "\\x y. x + y" `shouldParse` LambdaAST ["x", "y"]
        (Expr [Symbol "+", Symbol "x", Symbol "y"])
  describe "interpreter" $ do
    it "run expressions" $
      property $ \x y -> eval (Expr [Symbol "+", LitInt x, LitInt y]) `shouldBe` Right (Int (x + y))
