module Main where

import Parser (parse)
import Interpreter (eval, run)
import Control.Monad.Except (runExceptT)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.IO.Streams (stdin, stdout)

main :: IO ()
main = do
  args <- getArgs
  case parse (Text.pack $ head args) of
    Left err -> print err
    Right ast -> do
      result <- runExceptT (eval ast)
      case result of
        Left err -> print err
        Right val -> do
          result <- runExceptT (run stdin stdout val)
          case result of
            Left err -> print err
            Right _ -> return ()
