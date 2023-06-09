module Parser (parse, parser, AST (..)) where

import Control.Applicative (Alternative)
import Data.Void ( Void )
import Data.List (unwords, intercalate)
import Data.Foldable (foldr')
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
    ( choice,
      many,
      manyTill,
      between,
      (<|>),
      Parsec,
      MonadParsec(eof, notFollowedBy, try),
      some, sepBy, sepBy1 )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1, string )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(Prefix, InfixL) )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data AST
  = LitInt Integer
  | LitFloat Double
  | LitString Text
  | LitBool Bool
  | LitNull
  | Symbol Text
  | ArrayAST (Vector AST)
  | ObjectAST (Map Text AST)
  | LambdaAST (Vector Text) AST
  | Let (Vector (Text, Vector Text, AST)) AST
  | Expr (Vector AST)
  deriving Eq

instance Show AST where
  show e = case e of
    LitInt x -> show x
    LitFloat x -> show x
    LitString x -> show x
    LitBool True -> "true"
    LitBool False -> "false"
    LitNull -> "null"
    Symbol x -> Text.unpack x
    ArrayAST x -> '[' : intercalate ", " (map show $ Vec.toList x) ++ "]"
    ObjectAST x -> '{' : intercalate ", " (pairs $ Map.toList x) ++ "}"
    LambdaAST args ast -> '\\' : unwords (map Text.unpack $ Vec.toList args) ++ ". " ++ show ast
    Let xs ast -> "let " ++ intercalate ", " (bindings $ Vec.toList xs) ++ "; " ++  show ast
    Expr x -> '(' : unwords (map show $ Vec.toList x) ++ ")"
    where
      pairs = map (\(x, y) -> Text.unpack x ++ " = " ++ show y)
      bindings = map (\(x, y, z) -> Text.unpack x
                       ++ unwords (Vec.toList $ fmap Text.unpack y) ++ " = " ++ show z)

vecChoice :: Alternative m => Vector (m a) -> m a
vecChoice = choice

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = Text.pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
  
integer :: Parser Integer
integer = lexeme $ try (fmap negate $ char '-' >> L.decimal) <|> L.decimal

float :: Parser Double
float = lexeme $ try (fmap negate $ char '-' >> L.float) <|> L.float

keyword :: Text -> Parser Text
keyword keyword = try $ lexeme (string keyword <* notFollowedBy alphaNumChar)

keywords :: Set Text
keywords =
  [ "then"
  , "else"
  , "or"
  , "and"
  , "null"
  , "true"
  , "false"
  ]

identifier :: Parser Text
identifier = try $ lexeme $ do
  name <- fmap Text.pack ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if Set.member name keywords
    then fail "keyword"
    else return name

array :: Parser (Vector AST)
array = lexeme $ fmap Vec.fromList $ between (symbol "[") (symbol "]") $ sepBy expr (symbol ",")

object :: Parser (Map Text AST)
object = fmap Map.fromList $ between (symbol "{") (symbol "}") $ sepBy pair (symbol ",")
  where pair = do
          name <- identifier
          symbol "="
          ex <- expr
          return (name, ex)

let' :: Parser AST
let' = Let
  <$> fmap Vec.fromList (between (symbol "let") (symbol ";") $ sepBy1 binding (symbol ","))
  <*> pipeExpr
  where binding = do
          name <- identifier
          params <- Vec.fromList <$> many identifier
          symbol "="
          ex <- expr
          return (name, params, ex)
  
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

literal :: Parser AST
literal = vecChoice
  [ try $ fmap LitFloat float
  , fmap LitInt integer
  , fmap LitString stringLiteral
  , LitNull <$ keyword "null"
  , LitBool True  <$ keyword "true"
  , LitBool False  <$ keyword "false"
  , LitString <$> fmap Text.singleton charLiteral
  ]

exprList :: Parser AST
exprList = do
  f <- fmap Symbol identifier <|> parens pipeExpr
  args <- many $ vecChoice
    [ literal
    , fmap Symbol identifier 
    , fmap ArrayAST array
    , fmap ObjectAST object
    , lambda
    , let'
    , ifExpr
    , parens pipeExpr
    ]
  return $ case args of
    [] -> f
    x -> Expr $ Vec.cons f $ Vec.fromList x

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
lambda = LambdaAST
  <$> fmap Vec.fromList (between (symbol "\\") (symbol ".") $ some identifier)
  <*> expr

term :: Parser AST
term = vecChoice
  [ lambda
  , let'
  , ifExpr
  , fmap ArrayAST array
  , fmap ObjectAST object
  , literal
  , exprList
  ]

binary' :: Text -> Text -> Operator Parser AST
binary' name fname = InfixL ((\x y -> Expr [Symbol fname, x, y]) <$ symbol name)

binary :: Text -> Operator Parser AST
binary name = binary' name name

prefix' :: Text -> Text -> Operator Parser AST
prefix'  name fname = Prefix  ((\x -> Expr [Symbol fname, x]) <$ symbol name)

prefix :: Text -> Operator Parser AST
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
      , [ binary "=="
        , binary "!="
        , binary "<"
        , binary "<="
        , binary ">"
        , binary ">="
        ]
      , [ binary "and" ]
      , [ binary "or" ]
      ]

pipeExpr :: Parser AST
pipeExpr = makeExprParser expr [[binary "|"]]

parser :: Parser AST
parser = pipeExpr <* eof

parse :: Text -> Either (MP.ParseErrorBundle Text Void) AST
parse = MP.parse parser ""
