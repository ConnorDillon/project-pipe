module Pipe (parser, AST (..)) where

import Data.Void ( Void )
import Data.List (unwords, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
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
  = LitInt Integer
  | LitFloat Double
  | LitString String
  | LitBool Bool
  | LitNull
  | Symbol String
  | ArrayAST [AST]
  | ObjectAST (Map String AST)
  | LambdaAST [String] AST
  | Expr [AST]
  deriving Eq

instance Show AST where
  show e = case e of
    LitInt x -> show x
    LitFloat x -> show x
    LitString x -> show x
    LitBool True -> "true"
    LitBool False -> "false"
    LitNull -> "null"
    Symbol x -> x
    ArrayAST x -> '[' : intercalate ", " (map show x) ++ "]"
    ObjectAST x -> let pairs = map (\(x, y) -> x ++ " = " ++ show y) $ Map.toList x
                   in '{' : intercalate ", " pairs ++ "}"
    LambdaAST args ex -> '\\' : unwords args ++ ". " ++ show ex
    Expr x -> '(' : unwords (map show x) ++ ")"

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
literal = choice
  [ try $ fmap LitFloat float
  , fmap LitInt integer
  , fmap LitString stringLiteral
  , LitNull <$ keyword "null"
  , LitBool True  <$ keyword "true"
  , LitBool False  <$ keyword "false"
  , LitString <$> fmap pure charLiteral
  ]

exprList :: Parser AST
exprList = do
  f <- fmap Symbol identifier <|> parens expr
  args <- many $ do
    literal <|> fmap Symbol identifier <|> parens expr
  return $ case args of
    [] -> f
    x -> Expr (f:x)

ifExpr :: Parser AST
ifExpr = try $ do
  symbol "if"
  cond <- expr
  symbol "then"
  true <- expr
  symbol "else"
  false <- expr
  return $ Expr [Symbol "if", cond, true, false]

lambda :: Parser AST
lambda = LambdaAST <$> lexeme (between (symbol "\\") (symbol ".") $ some identifier) <*> expr

term :: Parser AST
term = choice
  [ lambda
  , ifExpr
  , literal
  , exprList
  ]

binary' :: String -> String -> Operator Parser AST
binary' name fname = InfixL ((\x y -> Expr [Symbol fname, x, y]) <$ symbol name)

binary :: String -> Operator Parser AST
binary name = binary' name name

prefix' :: String -> String -> Operator Parser AST
prefix'  name fname = Prefix  ((\x -> Expr [Symbol fname, x]) <$ symbol name)

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
