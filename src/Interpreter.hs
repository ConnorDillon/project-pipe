module Interpreter (eval, Value (..), Lambda (..)) where

import Parser (AST(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector, (!))
import Data.Foldable (foldr')
import Control.Monad ((>=>), filterM)
import qualified Data.Vector as Vec

data Value
  = Int Integer
  | Float Double
  | String Text
  | Bool Bool
  | Null
  | Stream [Value]
  | Array (Vector Value)
  | Object (Map Text Value)
  | Lambda Lambda
  deriving (Show, Eq, Ord)

data Lambda
  = Lambda1 (Value -> Either Text Value)
  | Lambda2 (Value -> Value -> Either Text Value)
  | Lambda3 (Value -> Value -> Value -> Either Text Value)

instance Show Lambda where
  show x = case x of
    Lambda1 _ -> "lambda1"
    Lambda2 _ -> "lambda2"
    Lambda3 _ -> "lambda3"

instance Ord Lambda where
  x <= y = show x <= show y

instance Eq Lambda where
  x == y = show x == show y

apply :: Lambda -> Vector Value -> Either Text Value
apply lambda vals = case lambda of
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

rewriteAST :: AST -> AST
rewriteAST (Let bindings ast) = foldr' toLambda ast bindings
  where
    toLambda (name, params, val) ast = let
      arg = if Vec.null params
        then rewriteAST val
        else LambdaAST params $ rewriteAST val
      in Expr [LambdaAST [name] (rewriteAST ast), arg]
rewriteAST (LambdaAST x ast) = LambdaAST x $ rewriteAST ast
rewriteAST (Expr x) = Expr $ fmap rewriteAST x
rewriteAST (ArrayAST x) = ArrayAST $ fmap rewriteAST x
rewriteAST (ObjectAST x) = ObjectAST $ fmap rewriteAST x
rewriteAST x = x

eval' :: Map Text Value -> AST -> Either Text Value
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
  Let _ _ -> Left "Can't eval let statement"
  LambdaAST params bodyAST -> case params of
    [p] -> return $ Lambda $ Lambda1 $ \v ->
      eval' (Map.insert p v symbols) bodyAST
    [p1, p2] -> return $ Lambda $ Lambda2 $ \v1 v2 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2)]) symbols) bodyAST
    [p1, p2, p3] -> return $ Lambda $ Lambda3 $ \v1 v2 v3 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2), (p3, v3)]) symbols) bodyAST
    [] -> Left "Lambda without params"
  Expr x -> case Vec.uncons x of
    Nothing -> Left "Empty expr"
    Just (lambdaAST, valsAST) -> do
      lambda <- eval' symbols lambdaAST
      case lambda of
        Lambda l -> mapM (eval' symbols) valsAST >>= apply l
        _ -> Left "Cannot apply to non-lambda value"

globals :: Map Text Value
globals = Map.fromList $ map (fmap Lambda)
  [ ("negate", Lambda1 negate')
  , ("+", Lambda2 add)
  , ("-", Lambda2 subtract')
  , ("/", Lambda2 divide)
  , ("*", Lambda2 multiply)
  , ("==", Lambda2 eq)
  , ("!=", Lambda2 neq)
  , ("<", Lambda2 lt)
  , ("<=", Lambda2 lte)
  , (">", Lambda2 gt)
  , (">=", Lambda2 gte)
  , ("|", Lambda2 pipe)
  , ("and", Lambda2 and')
  , ("or", Lambda2 or')
  , ("if", Lambda3 if')
  , ("map", Lambda2 map')
  , ("filter", Lambda2 filter')
  ]

eval :: AST -> Either Text Value
eval = eval' globals . rewriteAST

negate' :: Value -> Either Text Value
negate' (Int x) = return $ Int $ negate x
negate' (Float x) = return $ Float $ negate x
negate' _ = Left "Type error" 

add :: Value -> Value -> Either Text Value
add (Int x) (Int y) = return $ Int $ x + y
add (Float x) (Int y) = return $ Float $ x + fromIntegral y
add (Int x) (Float y) = return $ Float $ fromIntegral x + y
add (Float x) (Float y) = return $ Float $ x + y
add _ _ = Left "Type error"

subtract' :: Value -> Value -> Either Text Value
subtract' (Int x) (Int y) = return $ Int $ x - y
subtract' (Float x) (Int y) = return $ Float $ x - fromIntegral y
subtract' (Int x) (Float y) = return $ Float $ fromIntegral x - y
subtract' (Float x) (Float y) = return $ Float $ x - y
subtract' _ _ = Left "Type error"

divide :: Value -> Value -> Either Text Value
divide (Int x) (Int y) = return $ Float $ fromIntegral x / fromIntegral y
divide (Float x) (Int y) = return $ Float $ x / fromIntegral y
divide (Int x) (Float y) = return $ Float $ fromIntegral x / y
divide (Float x) (Float y) = return $ Float $ x / y
divide _ _ = Left "Type error"

multiply :: Value -> Value -> Either Text Value
multiply (Int x) (Int y) = return $ Int $ x * y
multiply (Float x) (Int y) = return $ Float $ x * fromIntegral y
multiply (Int x) (Float y) = return $ Float $ fromIntegral x * y
multiply (Float x) (Float y) = return $ Float $ x * y
multiply _ _ = Left "Type error"

if' :: Value -> Value -> Value -> Either Text Value
if' (Bool True) ex _ = return ex
if' (Bool False) _ ex = return ex
if' Null _ ex = return ex
if' _ _ _ = Left "Type error"

map' :: Value -> Value -> Either Text Value
map' (Lambda (Lambda1 fn)) (Stream x) = Stream <$> mapM fn x
map' (Lambda (Lambda1 fn)) (Array x) = Array <$> mapM fn x
map' (Lambda (Lambda1 fn)) (Object x) = Object <$> mapM fn x
map' _ _ = Left "Type error"

toBool :: Value -> Either Text Bool
toBool (Bool x) = return x
toBool Null = return False
toBool _ = Left "Type error"

filter' :: Value -> Value -> Either Text Value
filter' (Lambda (Lambda1 fn)) (Stream x) = Stream <$> filterM (fn >=> toBool) x
filter' (Lambda (Lambda1 fn)) (Array x) = Array <$> Vec.filterM (fn >=> toBool) x
filter' (Lambda (Lambda1 fn)) (Object x) = Object . Map.fromList <$>
  filterM (fn . snd >=> toBool) (Map.toList x)
filter' _ _ = Left "Type error"

eq :: Value -> Value -> Either Text Value
eq x y = return $ Bool $ x == y

gt :: Value -> Value -> Either Text Value
gt x y = return $ Bool $ x > y

gte :: Value -> Value -> Either Text Value
gte x y = return $ Bool $ x >= y

lt :: Value -> Value -> Either Text Value
lt x y = return $ Bool $ x < y

lte :: Value -> Value -> Either Text Value
lte x y = return $ Bool $ x <= y

neq :: Value -> Value -> Either Text Value
neq x y = return $ Bool $ x /= y

and' :: Value -> Value -> Either Text Value
and' x y = fmap Bool $ (&&) <$> toBool x <*> toBool y

or' :: Value -> Value -> Either Text Value
or' x y = fmap Bool $ (||) <$> toBool x <*> toBool y

not' :: Value -> Either Text Value
not' x = Bool . not <$> toBool x

pipe :: Value -> Value -> Either Text Value
pipe x (Lambda fn) = apply fn [x]
pipe _ _ = Left "Type error"
