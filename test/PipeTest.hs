module Main (main) where

import Pipe ( parser, Expr(..) )
import Data.Void ( Void )
import qualified Text.Megaparsec as MP
import Test.QuickCheck ( Testable(property) )
import Test.Hspec ( hspec, describe, it )
import Test.Hspec.Megaparsec ( shouldFailOn, shouldParse )

parse :: String -> Either (MP.ParseErrorBundle String Void) Expr
parse = MP.parse parser ""

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "parses integers" $
      property $ \x -> parse (show x) `shouldParse` Int x
    it "parses floats" $
      property $ \x -> parse (show x) `shouldParse` Float x
    it "parses strings" $
      property $ \x -> parse (show x) `shouldParse` String x
    it "parses booleans" $ do
      parse "true" `shouldParse` Bool True
      parse "false" `shouldParse` Bool False
    it "parses null" $
      parse "null" `shouldParse` Null
    it "parses symbols" $ do
      parse "nullFoo" `shouldParse` Symbol "nullFoo"
      parse "foo1" `shouldParse` Symbol "foo1"
      parse "foo_bar" `shouldParse` Symbol "foo_bar"
      parse "BAR" `shouldParse` Symbol "BAR"
    it "repects keywords" $
      parse `shouldFailOn` "then"
    it "parses if statements" $
      parse "if true then 1 else 2" `shouldParse` Expr [Symbol "if" , Bool True , Int 1 , Int 2]
    it "parses expressions" $
      parse "foo 1 2 + bar 3" `shouldParse` Expr
        [ Symbol "+"
        , Expr [Symbol "foo", Int 1, Int 2]
        , Expr [Symbol "bar", Int 3]
        ]
    it "parses lambdas" $
      parse "\\x y. x + y" `shouldParse` Lambda ["x", "y"] (Expr [Symbol "+", Symbol "x", Symbol "y"])
