{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Text.Trifecta
import Data.Char (isSpace)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import System.IO

data Expr' a = Lit Integer | Var String | List [a] deriving (Eq, Show)

instance Functor Expr' where
  fmap f (List xs) = List $ map f xs

data Fix f = Fix (f (Fix f))

instance Eq (Fix Expr') where
  (==) (Fix (Var x)) (Fix (Var y)) = x == y
  (==) (Fix (Lit x)) (Fix (Lit y)) = x == y
  (==) (Fix (List xs)) (Fix (List ys)) = if length xs /= length ys then False 
                                         else 
                                          foldr (&&) True $ zipWith (==) xs ys
  (==) _ _ = False

instance Show (Fix Expr') where
  show (Fix (Lit x)) = "Lit " ++ show x
  show (Fix (Var x)) = "Var " ++ show x
  show (Fix (List xs)) = "(" ++ intercalate ", " (map show xs) ++ ")"

validChar :: Parser Char
validChar = satisfy $ \x -> not (isSpace x) && x /= ')' && x /= '(' 

var :: Parser (Fix Expr')
var = fmap (Fix . Var) $ some validChar

lit :: Parser (Fix Expr')
lit = fmap (Fix . Lit) $ natural <|> char '-' *> fmap negate natural

list :: Parser (Fix Expr')
list = fmap (Fix . List) $ parens $ 
        whiteSpace *> some (lit <|> token var <|> token list)

file :: Parser [Fix Expr']
file = sepBy (token list) newline

parseFile :: IO ()
parseFile = parseFromFile file "samplecode" >>= print
