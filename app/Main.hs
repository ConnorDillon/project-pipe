module Main where

import qualified Pipe (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Pipe.someFunc
