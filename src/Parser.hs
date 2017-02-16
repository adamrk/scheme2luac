{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Text.Trifecta
import Data.Char (isSpace)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import System.IO

data Value = Number Integer 
           | Atom String
           | List [Value]
           deriving (Eq, Show)

validChar :: Parser Char
validChar = satisfy $ \x -> not (isSpace x) && x /= ')' && x /= '(' 

atom :: Parser Value
atom = fmap Atom $ some validChar

number :: Parser Value
number = fmap Number $ natural <|> char '-' *> fmap negate natural

list :: Parser Value
list = fmap List $ parens $ 
        whiteSpace *> some (token number <|> token atom <|> token list)

file :: Parser [Value]
file = sepBy (token list) newline

parseFile :: IO ()
parseFile = parseFromFile file "samplecode" >>= print