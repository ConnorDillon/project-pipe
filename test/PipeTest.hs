module Main (main) where

import Pipe ( parser, AST(..) )
import Interpreter ( Value(..), eval )
import Data.Void ( Void )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Text.Megaparsec as MP
import Test.QuickCheck ( Testable(property) )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.Megaparsec ( shouldFailOn, shouldParse )

parse :: Text -> Either (MP.ParseErrorBundle Text Void) AST
parse = MP.parse parser ""

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "parses integers" $
      property $ \x -> parse (Text.pack $ show x) `shouldParse` LitInt x
    it "parses floats" $
      property $ \x -> parse (Text.pack $ show x) `shouldParse` LitFloat x
    it "parses strings" $
      property $ \x -> parse (Text.pack $ show x) `shouldParse` LitString (Text.pack x)
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
      property $ \x y -> let expr = Expr [Symbol "+", LitInt x, LitInt y]
                         in eval expr `shouldBe` Right (Int (x + y))
