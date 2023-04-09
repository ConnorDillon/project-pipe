module Interpreter (eval, Value (..), Lambda (..)) where

import Pipe ( AST(..) )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq(Empty))

data Value
  = Int Integer
  | Float Double
  | String String
  | Bool Bool
  | Null
  | Stream [Value]
  | Array [Value]
  | Object (Map String Value)
  | Lambda Lambda
  deriving (Show, Eq)

data Lambda
  = Lambda1 (Value -> Either String Value)
  | Lambda2 (Value -> Value -> Either String Value)
  | Lambda3 (Value -> Value -> Value -> Either String Value)

instance Show Lambda where
  show = undefined

instance Eq Lambda where
  (==) = undefined

apply :: Map String Value -> Lambda -> [Value] -> Either String Value
apply symbols lambda vals = case lambda of
  Lambda1 fn -> case vals of
    [val] -> fn val
    _ -> badArgs
  Lambda2 fn -> case vals of
    [v1, v2] -> fn v1 v2
    [v1] -> return $ Lambda $ Lambda1 $ fn v1
    _ -> badArgs
  Lambda3 fn -> case vals of
    [v1, v2, v3] -> fn v1 v2 v3
    [v1, v2] -> return $ Lambda $ Lambda1 $ fn v1 v2
    [v1] -> return $ Lambda $ Lambda2 $ fn v1
    _ -> badArgs
  where
    badArgs = Left "Incorrect number of arguments passed to function"

eval' :: Map String Value -> AST -> Either String Value
eval' symbols ast = case ast of
  LitInt x -> return $ Int x
  LitFloat x -> return $ Float x
  LitString x -> return $ String x
  LitBool x -> return $ Bool x
  LitNull -> return Null
  ArrayAST arr -> Array <$> mapM (eval' symbols) arr
  ObjectAST obj -> Object <$> mapM (eval' symbols) obj
  Symbol name -> do
    case Map.lookup name symbols of
      Just val -> return val
      Nothing -> Left "Symbol not found"
  LambdaAST params bodyAST -> case params of
    [p] -> return $ Lambda $ Lambda1 $ \v ->
      eval' (Map.insert p v symbols) bodyAST
    [p1, p2] -> return $ Lambda $ Lambda2 $ \v1 v2 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2)]) symbols) bodyAST
    [p1, p2, p3] -> return $ Lambda $ Lambda3 $ \v1 v2 v3 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2), (p3, v3)]) symbols) bodyAST
    [] -> Left "Lambda without params"
  Expr [] -> Left "Empty expr"
  Expr (lambdaAST : valsAST) -> do
    lambda <- eval' symbols lambdaAST
    case lambda of
      Lambda l -> mapM (eval' symbols) valsAST >>= apply symbols l
      _ -> Left "Cannot apply to non-lambda value"

globals :: Map String Value
globals = Map.fromList $ map (fmap Lambda)
  [ ("negate", Lambda1 negate')
  , ("+", Lambda2 add)
  , ("-", Lambda2 subtract')
  , ("/", Lambda2 divide)
  , ("*", Lambda2 multiply)
  , ("if", Lambda3 if')
  ]

eval :: AST -> Either String Value
eval = eval' globals

negate' :: Value -> Either String Value
negate' (Int x) = return $ Int $ negate x
negate' (Float x) = return $ Float $ negate x
negate' _ = Left "Type error" 

add :: Value -> Value -> Either String Value
add (Int x) (Int y) = return $ Int $ x + y
add (Float x) (Int y) = return $ Float $ x + fromIntegral y
add (Int x) (Float y) = return $ Float $ fromIntegral x + y
add (Float x) (Float y) = return $ Float $ x + y
add _ _ = Left "Type error"

subtract' :: Value -> Value -> Either String Value
subtract' (Int x) (Int y) = return $ Int $ x - y
subtract' (Float x) (Int y) = return $ Float $ x - fromIntegral y
subtract' (Int x) (Float y) = return $ Float $ fromIntegral x - y
subtract' (Float x) (Float y) = return $ Float $ x - y
subtract' _ _ = Left "Type error"

divide :: Value -> Value -> Either String Value
divide (Int x) (Int y) = return $ Float $ fromIntegral x / fromIntegral y
divide (Float x) (Int y) = return $ Float $ x / fromIntegral y
divide (Int x) (Float y) = return $ Float $ fromIntegral x / y
divide (Float x) (Float y) = return $ Float $ x / y
divide _ _ = Left "Type error"

multiply :: Value -> Value -> Either String Value
multiply (Int x) (Int y) = return $ Int $ x * y
multiply (Float x) (Int y) = return $ Float $ x * fromIntegral y
multiply (Int x) (Float y) = return $ Float $ fromIntegral x * y
multiply (Float x) (Float y) = return $ Float $ x * y
multiply _ _ = Left "Type error"

if' :: Value -> Value -> Value -> Either String Value
if' (Bool True) ex _ = return ex
if' (Bool False) _ ex = return ex
if' Null _ ex = return ex
if' _ _ _ = Left "Type error"
