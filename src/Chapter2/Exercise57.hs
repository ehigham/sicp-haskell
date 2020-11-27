module Chapter2.Exercise57 (parseExpr) where

import Control.Applicative ((<|>))

import Text.Parsec.Char (spaces)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.Prim (runParser)
import Text.Parsec.String (Parser)

import Chapter2.Symbolic
    (
        Expr,
        add,
        antiexpr,
        constant,
        divide,
        multiply,
        parens,
        power,
        sub,
        variable
    )

-- | Extend the differentiation program to handle sums and products of arbitrary
-- number of (two or more) terms. Then the last example could be expressed as
-- @
--     deriv [lisp|(* x y (+ x 3))|] "x"
-- @
--
-- Try to do this by changing only the representation for sums and products
-- without changing the `deriv` procedure at all. For example, the `addend` of
-- a sum would be the first term and teh `augend` would be the rest of the
-- terms.
--
-- Haskell doesn't really have functions that take arbitrary numbers of
-- arguments. We can create a simple parser for lisp expressions, however.

parseExpr :: MonadFail m => String -> m Expr
parseExpr s = case runParser p () "" s of
    Left err -> fail $ show err
    Right e  -> return e
  where
    p = do spaces
           e <- expression
           eof
           return e

expression :: Parser Expr
expression = constant <|> variable <|> antiexpr <|> parens apply
  where
    apply = do f  <- add <|> multiply <|> power <|> sub <|> divide
               x  <- expression
               xs <- many1 expression
               return $ foldl f x xs
