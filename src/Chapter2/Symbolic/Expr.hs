{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Chapter2.Symbolic.Expr (
    Expr(..),
    BinaryOp(..),
    expr,
    parseExpr
) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Data (Data)
import Data.Functor ((<&>))
import Data.Generics.Aliases (extQ)

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH.Syntax as TH

import Text.Parsec.Char (alphaNum, digit, spaces, string, char)
import Text.Parsec.Combinator (between, chainl1, eof, many1)
import Text.Parsec.Prim (runParser)
import Text.Parsec.String (Parser)

data Expr = Const Integer
          | Var String
          | AntiExpr String
          | UnaryExpr UnaryOp Expr
          | BinaryExpr BinaryOp Expr Expr
  deriving stock (Eq, Show, Data)

data UnaryOp = Plus
             | Minus
             | Ln
  deriving stock (Eq, Show, Data)

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | Exp
  deriving stock (Eq, Show, Data)

expr :: QuasiQuoter
expr = QuasiQuoter
    { quoteExp  = TH.dataToExpQ (const Nothing `extQ` antiExprExp) <=< parseExpr
    , quotePat  = TH.dataToPatQ (const Nothing `extQ` antiExprPat) <=< parseExpr
    , quoteType = const (error "cannot quote type")
    , quoteDec = const (error "cannot quote declaration")
    }

antiExprExp :: Expr -> Maybe (TH.Q TH.Exp)
antiExprExp (AntiExpr v) = Just . return $ TH.VarE  (TH.mkName v)
antiExprExp _            = Nothing

antiExprPat :: Expr -> Maybe (TH.Q TH.Pat)
antiExprPat (AntiExpr v) = Just . return $ TH.VarP  (TH.mkName v)
antiExprPat _            = Nothing

parseExpr :: MonadFail m => String -> m Expr
parseExpr s = case runParser p () "" s of
    Left err -> fail $ show err
    Right e  -> return e
  where
    p = do { e <- expression ; eof ; return e }

lexeme :: Parser a -> Parser a
lexeme p = do { x <- p ; spaces ; return x }

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

expression, term, factor, unary, leaf :: Parser Expr
expression = term `chainl1` (add <|> sub)
  where
    add = symbol "+" >> return (BinaryExpr Add)
    sub = symbol "-" >> return (BinaryExpr Sub)

term = factor `chainl1` (multiply <|> divide)
  where
    multiply = symbol "*" >> return (BinaryExpr Mul)
    divide   = symbol "/" >> return (BinaryExpr Div)

factor = unary `chainl1` raise
  where
    raise = symbol "^" >> return (BinaryExpr Exp)

unary = (plus <|> minus <|> ln) <*> leaf <|> leaf
  where
    plus  = symbol "+" >> return (UnaryExpr Plus)
    minus = symbol "-" >> return (UnaryExpr Minus)
    ln    = symbol "ln " >> return (UnaryExpr Ln)

leaf = parens expression <|> integer <|> variable <|> antiexpr
  where
    integer  = lexeme $ many1 digit <&> Const . read
    variable = lexeme $ identifier <&> Var
    antiexpr = lexeme $ symbol "$" >> identifier <&> AntiExpr

identifier :: Parser String
identifier = many1 (alphaNum <|> char '_' <|> char '\'')
