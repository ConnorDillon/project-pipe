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

shouldEval x y = fmap eval (parse x) `shouldParse` Right y

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
    it "parses expressions" $ do
      parse "not x and false or y > z and true" `shouldParse` Expr
        [ Symbol "or"
        , Expr [Symbol "and", Expr [Symbol "not", Symbol "x"], LitBool False]
        , Expr
            [ Symbol "and"
            , Expr [Symbol ">", Symbol "y", Symbol "z"]
            , LitBool True
            ]
        ]
      parse "foo 1 2 + bar 3" `shouldParse` Expr
        [ Symbol "+"
        , Expr [Symbol "foo", LitInt 1, LitInt 2]
        , Expr [Symbol "bar", LitInt 3]
        ]
      parse "2 + (3 - 1) * 2" `shouldParse` Expr
        [ Symbol "+"
        , LitInt 2
        , Expr
            [ Symbol "*"
            , Expr [Symbol "-", LitInt 3, LitInt 1]
            , LitInt 2
            ]
        ]
    it "parses lambdas" $
      parse "\\x y. x + y" `shouldParse` LambdaAST ["x", "y"]
        (Expr [Symbol "+", Symbol "x", Symbol "y"])
    it "parses arrays" $ do
      parse "[]" `shouldParse` ArrayAST []
      parse "[1, 1.5 * 2, foo, \\x.x]" `shouldParse` ArrayAST
        [ LitInt 1
        , Expr [Symbol "*", LitFloat 1.5, LitInt 2]
        , Symbol "foo"
        , LambdaAST ["x"] (Symbol "x")
        ]
      parse "[[1]]" `shouldParse` ArrayAST [ArrayAST [LitInt 1]]
    it "parses objects" $ do
      parse "{}" `shouldParse` ObjectAST []
      parse "{foo = \"bar\", baz = 1}" `shouldParse` ObjectAST
        [ ("foo", LitString "bar")
        , ("baz", LitInt 1)
        ]
      parse "{foo = [{bar = 0}]}" `shouldParse`
        ObjectAST [("foo", ArrayAST [ObjectAST [("bar", LitInt 0)]])]
    it "parses let statements" $ do
      parse `shouldFailOn` "let; foo"
      parse "let foo = 1, bar = [foo]; bar * 2" `shouldParse` Let
        [ ("foo", [], LitInt 1)
        , ("bar", [], ArrayAST [Symbol "foo"])
        ]
        (Expr [Symbol "*", Symbol "bar", LitInt 2])
  describe "interpreter" $ do
    it "run expressions" $
      property $ \x y -> let expr = Expr [Symbol "+", LitInt x, LitInt y]
                         in eval expr `shouldBe` Right (Int (x + y))
    it "run pipe expressions" $ do
      "map (\\x. x + 1) (filter (\\x. x < 3) [1, 2, 3])"
        `shouldEval` Array [Int 2, Int 3]
      "[1, 2, 3] | filter \\x. x < 3 | map \\x. x + 1"
        `shouldEval` Array [Int 2, Int 3]
      "let foo x = x < 3, bar x = x + 1; [1, 2, 3] | filter foo | map bar"
        `shouldEval` Array [Int 2, Int 3]
      "let foo x = x < 3; [1, 2, 3] | \\x. let bar x = x + 1; x | filter foo | map bar"
        `shouldEval` Array [Int 2, Int 3]
      "let bar x = x + 1; [1, 2, 3] | (let foo x = x < 3; filter foo) | map bar"
        `shouldEval` Array [Int 2, Int 3]
    it "run let statements" $ do
      "let foo = 1, bar = foo; bar + 1" `shouldEval` Int 2
      "2 * let foo = 1; foo + 3" `shouldEval` Int (2 * let foo = 1 in foo + 3)
      "let foo = 1, bar = let baz = foo; baz; bar + 1" `shouldEval` Int 2
      "negate (let foo = 1; foo)" `shouldEval` Int (-1)
      "let foo x = 1 + x, bar = foo 2; foo bar" `shouldEval` Int 4
