module Main (main) where

import Pipe ( parser, AST(..), Value(..), LambdaAST )
import Data.Void ( Void )
import qualified Text.Megaparsec as MP
import Test.QuickCheck ( Testable(property) )
import Test.Hspec ( hspec, describe, it )
import Test.Hspec.Megaparsec ( shouldFailOn, shouldParse )

parse :: String -> Either (MP.ParseErrorBundle String Void) AST
parse = MP.parse parser ""

bool :: Bool -> AST
bool = Value . Bool

symbol :: String -> AST
symbol = Value . Symbol

int :: Integer -> AST
int = Value . Int

float :: Double -> AST
float = Value . Float

null' :: AST
null' = Value Null

string :: String -> AST
string = Value . String

array :: [Value LambdaAST] -> AST
array = Value . Array

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "parses integers" $
      property $ \x -> parse (show x) `shouldParse` int x
    it "parses floats" $
      property $ \x -> parse (show x) `shouldParse` float x
    it "parses strings" $
      property $ \x -> parse (show x) `shouldParse` string x
    it "parses booleans" $ do
      parse "true" `shouldParse` bool True
      parse "false" `shouldParse` bool False
    it "parses null" $
      parse "null" `shouldParse` null'
    it "parses symbols" $ do
      parse "nullFoo" `shouldParse` symbol "nullFoo"
      parse "foo1" `shouldParse` symbol "foo1"
      parse "foo_bar" `shouldParse` symbol "foo_bar"
      parse "BAR" `shouldParse` symbol "BAR"
    it "repects keywords" $
      parse `shouldFailOn` "then"
    it "parses if statements" $
      parse "if true then 1 else 2" `shouldParse` Expr [symbol "if" , bool True , int 1 , int 2]
    it "parses expressions" $
      parse "foo 1 2 + bar 3" `shouldParse` Expr
        [ symbol "+"
        , Expr [symbol "foo", int 1, int 2]
        , Expr [symbol "bar", int 3]
        ]
    it "parses lambdas" $
      parse "\\x y. x + y" `shouldParse` Expr
        [ symbol "lambda"
        , array [Symbol "x", Symbol "y"]
        , Expr [symbol "+", symbol "x", symbol "y"]
        ]
