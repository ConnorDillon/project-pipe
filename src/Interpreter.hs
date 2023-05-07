module Interpreter (eval, run, Value (..), Lambda (..), jsonInput, jsonOutput) where

import Parser (AST(..))
import ListT (ListT(..))
import qualified ListT as L
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (json)
import qualified Data.Aeson as JS
import Data.Aeson.KeyMap (toMapText, fromMapText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.Foldable (foldr')
import Data.Attoparsec.ByteString (endOfInput)
import Control.Applicative ((<|>))
import Control.Monad ((>=>), filterM, MonadPlus(mzero))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Fix (fix)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec.ByteString (parserToInputStream)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)

data Value m
  = Int Integer
  | Float Double
  | String Text
  | Bool Bool
  | Null
  | Stream (ListT m (Value m))
  | Array (Vector (Value m))
  | Object (Map Text (Value m))
  | Lambda (Lambda m)

instance Show (Value m) where
  show e = case e of
    Int x -> show x
    Float x -> show x
    String x -> show x
    Bool x -> show x
    Null -> "null"
    Stream x -> "<stream>"
    Array x -> show x
    Object x -> show x
    Lambda x -> show x

instance Ord (Value m) where
  x <= y = show x <= show y

instance Eq (Value m) where
  x == y = show x == show y

fromJSON :: JS.Value -> Value m
fromJSON v = case v of
  JS.Array x -> Array $ Vec.map fromJSON x
  JS.Object x -> Object $ toMapText $ KeyMap.map fromJSON x
  JS.Number x -> case floatingOrInteger x of
    Right i -> Int i
    Left f -> Float f
  JS.String x -> String x
  JS.Bool x -> Bool x
  JS.Null -> Null

tryToJSON :: MonadError Text m => Value m -> m JS.Value
tryToJSON v = case v of
  Int x -> return $ JS.Number $ fromInteger x
  Float x ->  return $ JS.Number $ fromFloatDigits x
  String x -> return $ JS.String x
  Bool x -> return $ JS.Bool x
  Null -> return JS.Null
  Stream x -> L.toList x >>= fmap JS.Array . mapM tryToJSON . Vec.fromList
  Array x ->  JS.Array <$> mapM tryToJSON x
  Object x ->  JS.Object . fromMapText <$> mapM tryToJSON x
  Lambda _ -> throwError "Can't encode lambda as JSON"

streamList :: (Show a, MonadIO m) => InputStream a -> ListT m a
streamList inp = fix $ \loop -> liftIO (Streams.read inp) >>=
  \x -> maybe mzero (`L.cons` loop) x

jsonInput :: MonadIO m => InputStream ByteString -> ListT m (Value m)
jsonInput inp = do
  s <- liftIO $ parserToInputStream ((endOfInput >> pure Nothing) <|> fmap Just json) inp
  fromJSON <$> streamList s

jsonOutput
  :: (MonadIO m, MonadError Text m)
  => OutputStream ByteString
  -> ListT m (Value m)
  -> m ()
jsonOutput out lst = do
  L.traverse_ (writeValue out) lst
  liftIO $ Streams.writeTo out Nothing 

writeValue :: (MonadError Text m, MonadIO m) => OutputStream ByteString -> Value m -> m ()
writeValue out val = do
   x <- tryToJSON val
   liftIO $ Streams.writeTo out $ Just $ BS.toStrict $ JS.encode x
   liftIO $ Streams.writeTo out $ Just $ encodeUtf8  "\n"

run
  :: (MonadIO m, MonadError Text m)
  => InputStream ByteString
  -> OutputStream ByteString
  -> Value m
  -> m ()
run inp out (Lambda (Lambda1 fn)) = do
  result <- fn $ Stream $ jsonInput inp 
  case result of
    Stream l -> jsonOutput out l
    v -> writeValue out v
run inp out val = do
  writeValue out val
  liftIO $ Streams.writeTo out Nothing 

type Result m = m (Value m)

data Lambda m
  = Lambda1 (Value m -> Result m)
  | Lambda2 (Value m -> Value m -> Result m)
  | Lambda3 (Value m -> Value m -> Value m -> Result m)

instance Show (Lambda m) where
  show x = case x of
    Lambda1 _ -> "<lambda1>"
    Lambda2 _ -> "<lambda2>"
    Lambda3 _ -> "<lambda3>"

instance Ord (Lambda m) where
  x <= y = show x <= show y

instance Eq (Lambda m) where
  x == y = show x == show y

apply :: MonadError Text m => Lambda m -> Vector (Value m) -> Result m
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
    badArgs = throwError "Incorrect number of arguments passed to function"

rewriteAST :: AST -> AST
rewriteAST (Let bindings ast) = foldr' toLambda ast bindings
  where
    toLambda (name, params, val) ast = let
      arg = if Vec.null params
        then rewriteAST val
        else LambdaAST params $ rewriteAST val
      in Expr [LambdaAST [name] (rewriteAST ast), arg]
rewriteAST (LambdaAST x ast) = LambdaAST x $ rewriteAST ast
-- rewriteAST (Expr [Symbol "|", x, fn]) = Expr [rewriteAST fn, rewriteAST x] 
rewriteAST (Expr x) = Expr $ fmap rewriteAST x
rewriteAST (ArrayAST x) = ArrayAST $ fmap rewriteAST x
rewriteAST (ObjectAST x) = ObjectAST $ fmap rewriteAST x
rewriteAST x = x

eval' :: MonadError Text m => Map Text (Value m) -> AST -> Result m
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
      Nothing -> throwError "Symbol not found"
  Let _ _ -> throwError "Can't eval let statement"
  LambdaAST params bodyAST -> case params of
    [p] -> return $ Lambda $ Lambda1 $ \v ->
      eval' (Map.insert p v symbols) bodyAST
    [p1, p2] -> return $ Lambda $ Lambda2 $ \v1 v2 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2)]) symbols) bodyAST
    [p1, p2, p3] -> return $ Lambda $ Lambda3 $ \v1 v2 v3 ->
      eval' (Map.union (Map.fromList [(p1, v1), (p2, v2), (p3, v3)]) symbols) bodyAST
    [] -> throwError "Lambda without params"
  Expr x -> case Vec.uncons x of
    Nothing -> throwError "Empty expr"
    Just (lambdaAST, valsAST) -> do
      lambda <- eval' symbols lambdaAST
      case lambda of
        Lambda l -> mapM (eval' symbols) valsAST >>= apply l
        _ -> throwError "Cannot apply to non-lambda value"

globals :: MonadError Text m => Map Text (Value m)
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
  , ("flatten", Lambda1 flatten)
  , ("array", Lambda1 array)
  , ("type", Lambda1 type')
  ]

eval :: MonadError Text m => AST -> Result m
eval = eval' globals . rewriteAST

negate' :: MonadError Text m => Value m -> Result m
negate' (Int x) = return $ Int $ negate x
negate' (Float x) = return $ Float $ negate x
negate' _ = throwError "Type error" 

add :: MonadError Text m => Value m -> Value m -> Result m
add (Int x) (Int y) = return $ Int $ x + y
add (Float x) (Int y) = return $ Float $ x + fromIntegral y
add (Int x) (Float y) = return $ Float $ fromIntegral x + y
add (Float x) (Float y) = return $ Float $ x + y
add _ _ = throwError "Type error"

subtract' :: MonadError Text m => Value m -> Value m -> Result m
subtract' (Int x) (Int y) = return $ Int $ x - y
subtract' (Float x) (Int y) = return $ Float $ x - fromIntegral y
subtract' (Int x) (Float y) = return $ Float $ fromIntegral x - y
subtract' (Float x) (Float y) = return $ Float $ x - y
subtract' _ _ = throwError "Type error"

divide :: MonadError Text m => Value m -> Value m -> Result m
divide (Int x) (Int y) = return $ Float $ fromIntegral x / fromIntegral y
divide (Float x) (Int y) = return $ Float $ x / fromIntegral y
divide (Int x) (Float y) = return $ Float $ fromIntegral x / y
divide (Float x) (Float y) = return $ Float $ x / y
divide _ _ = throwError "Type error"

multiply :: MonadError Text m => Value m -> Value m -> Result m
multiply (Int x) (Int y) = return $ Int $ x * y
multiply (Float x) (Int y) = return $ Float $ x * fromIntegral y
multiply (Int x) (Float y) = return $ Float $ fromIntegral x * y
multiply (Float x) (Float y) = return $ Float $ x * y
multiply _ _ = throwError "Type error"

if' :: MonadError Text m => Value m -> Value m -> Value m -> Result m
if' (Bool True) ex _ = return ex
if' (Bool False) _ ex = return ex
if' Null _ ex = return ex
if' _ _ _ = throwError "Type error"

map' :: MonadError Text m => Value m -> Value m -> Result m
map' (Lambda (Lambda1 fn)) (Stream x) = return $ Stream $ L.traverse fn x
map' (Lambda (Lambda1 fn)) (Array x) = Array <$> mapM fn x
map' (Lambda (Lambda1 fn)) (Object x) = Object <$> mapM fn x
map' _ _ = throwError "Type error"

toBool :: MonadError Text m => Value m -> m Bool
toBool (Bool x) = return x
toBool Null = return False
toBool _ = throwError "Type error"

filterLT :: Monad m => (a -> m Bool) -> ListT m a -> ListT m a
filterLT fn l = do
  x <- l
  cond <- lift $ fn x
  if cond then return x else mempty

filter' :: MonadError Text m => Value m -> Value m -> Result m
filter' (Lambda (Lambda1 fn)) (Stream x) = return $ Stream $ filterLT (fn >=> toBool) x
filter' (Lambda (Lambda1 fn)) (Array x) = Array <$> Vec.filterM (fn >=> toBool) x
filter' (Lambda (Lambda1 fn)) (Object x) = Object . Map.fromList <$>
  filterM (fn . snd >=> toBool) (Map.toList x)
filter' _ _ = throwError "Type error"

flattenLT :: Monad m => ListT m (Value m) -> ListT m (Value m)
flattenLT l = do
  x <- l
  case x of
    Stream s -> s
    Array a -> L.fromFoldable a
    v -> return v

flatten :: MonadError Text m => Value m -> Result m
flatten (Stream s) = return $ Stream $ flattenLT s
flatten (Array a) = return $ Stream $ flattenLT $ L.fromFoldable a
flatten _ = throwError "Type error"

array :: MonadError Text m => Value m -> Result m
array (Stream s) = Array <$> Vec.unfoldrM L.uncons s
array a@(Array _) = return a
array _ = throwError "Type error"

eq :: MonadError Text m => Value m -> Value m -> Result m
eq x y = return $ Bool $ x == y

gt :: MonadError Text m => Value m -> Value m -> Result m
gt x y = return $ Bool $ x > y

gte :: MonadError Text m => Value m -> Value m -> Result m
gte x y = return $ Bool $ x >= y

lt :: MonadError Text m => Value m -> Value m -> Result m
lt x y = return $ Bool $ x < y

lte :: MonadError Text m => Value m -> Value m -> Result m
lte x y = return $ Bool $ x <= y

neq :: MonadError Text m => Value m -> Value m -> Result m
neq x y = return $ Bool $ x /= y

and' :: MonadError Text m => Value m -> Value m -> Result m
and' x y = fmap Bool $ (&&) <$> toBool x <*> toBool y

or' :: MonadError Text m => Value m -> Value m -> Result m
or' x y = fmap Bool $ (||) <$> toBool x <*> toBool y

not' :: MonadError Text m => Value m -> Result m
not' x = Bool . not <$> toBool x

pipe :: MonadError Text m => Value m -> Value m -> Result m
pipe (Lambda (Lambda1 x)) (Lambda (Lambda1 y)) = return $ Lambda $ Lambda1 $ x >=> y 
pipe (Lambda _) _ = throwError "Type error"
pipe x (Lambda (Lambda1 y)) = y x
pipe _ _ = throwError "Type error"

type' :: Monad m => Value m -> Result m
type' (Int _) = return $ String "String"  
type' (Float _) = return $ String "Float"  
type' (String _) = return $ String "String"  
type' (Bool _) = return $ String "Bool"  
type' Null = return $ String "Null"  
type' (Stream _) = return $ String "Stream"  
type' (Array _) = return $ String "Array"  
type' (Object _) = return $ String "Object"  
type' (Lambda _) = return $ String "Lambda"  
