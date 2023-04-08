module Pipe (parser, AST (..), Value (..), LambdaAST (..)) where

import Data.Void ( Void )
import Data.List (unwords, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
    ( choice,
      many,
      manyTill,
      between,
      (<|>),
      Parsec,
      MonadParsec(eof, notFollowedBy, try),
      some )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1, string )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(Prefix, InfixL) )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data AST
  = Value (Value LambdaAST)
  | Expr [AST]
  deriving Eq

instance Show AST where
  show t = case t of
    Value x -> show x
    Expr x -> '(' : unwords (map show x) ++ ")"

data Value a
  = Int Integer
  | Float Double
  | String String
  | Bool Bool
  | Null
  | Symbol String
  | Array [Value a]
  | Lambda a
  deriving Eq

data LambdaAST = LambdaAST [String] AST deriving (Show, Eq)

instance Show a => Show (Value a) where
  show e = case e of
    Int x -> show x
    Float x -> show x
    String x -> show x
    Bool True -> "true"
    Bool False -> "false"
    Null -> "null"
    Symbol x -> x
    Array x -> '[' : intercalate ", " (map show x) ++ "]"
    Lambda x -> show x

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')
  
integer :: Parser Integer
integer = lexeme $ try (fmap negate $ char '-' >> L.decimal) <|> L.decimal

float :: Parser Double
float = lexeme $ try (fmap negate $ char '-' >> L.float) <|> L.float

keyword :: String -> Parser String
keyword keyword = try $ lexeme (string keyword <* notFollowedBy alphaNumChar)

keywords :: Set String
keywords = Set.fromList
  [ "then"
  , "else"
  , "or"
  , "and"
  , "not"
  , "null"
  , "true"
  , "false"
  ]

identifier :: Parser String
identifier = try $ lexeme $ do
  name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if Set.member name keywords
    then fail "keyword"
    else return name

parens :: Parser a -> Parser a
parens = lexeme . between (symbol "(") (symbol ")")

literal :: Parser AST
literal = Value <$> choice
  [ try $ fmap Float float
  , fmap Int integer
  , fmap String stringLiteral
  , Null <$ keyword "null"
  , Bool True  <$ keyword "true"
  , Bool False  <$ keyword "false"
  , String <$> fmap pure charLiteral
  ]

exprList :: Parser AST
exprList = do
  f <- fmap (Value . Symbol) identifier <|> parens expr
  args <- many $ do
    literal <|> fmap (Value . Symbol) identifier <|> parens expr
  return $ case args of
    [] -> f
    _ -> Expr (f : args)

ifValue :: Parser AST
ifValue = try $ do
  symbol "if"
  cond <- expr
  symbol "then"
  true <- expr
  symbol "else"
  false <- expr
  return $ Expr [Value $ Symbol "if", cond, true, false]

lambda :: Parser AST
lambda = (\x y -> Expr [Value $ Symbol "lambda", Value $ Array $ fmap Symbol x, y])
  <$> lexeme (between (symbol "\\") (symbol ".") $ some identifier)
  <*> expr

term :: Parser AST
term = choice
  [ lambda
  , ifValue
  , literal
  , exprList
  ]

binary' :: String -> String -> Operator Parser AST
binary' name fname = InfixL ((\x y -> Expr [Value $ Symbol fname, x, y]) <$ symbol name)

binary :: String -> Operator Parser AST
binary name = binary' name name

prefix' :: String -> String -> Operator Parser AST
prefix'  name fname = Prefix  ((\x -> Expr [Value $ Symbol fname, x]) <$ symbol name)

prefix :: String -> Operator Parser AST
prefix name = prefix' name name
  
expr :: Parser AST
expr = makeExprParser term operatorTable
  where
    operatorTable =
      [ [ binary "*"
        , binary "/"
        ]
      , [ binary "+"
        , binary "-"
        ]
      , [ binary "or"
        , binary "and"
        ]
      ]

parser :: Parser AST
parser = expr <* eof
