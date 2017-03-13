{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Text.Trifecta
import Data.Char (isSpace, isAlpha, isAscii, isPrint)
import Data.List (intercalate)
import Control.Applicative ((<|>), liftA2, liftA3)
import Data.List (nub)
import System.IO
import Test.QuickCheck

data Value = Number Integer
           | Boolean Bool
           | Atom String
           | List [Value]
           deriving (Eq, Show)

-- randomAtom :: Gen String
-- randomAtom = (:) <$> arbitrary `suchThat` liftA2 (&&) isAlpha isAscii 
--            <*> (take 20 . filter (liftA2 (&&) isAscii validTest) <$> arbitrary)
randomVarAtom :: Gen Value
randomVarAtom = oneof $ map (pure . Atom) ["x", "y", "z", "foo", "bar"]

randomNonList :: Gen Value
randomNonList = oneof [ Number <$> arbitrary
                      , Boolean <$> arbitrary
                      , randomVarAtom
                      ]

listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- suchThat (choose (1, n)) (<= 4)
     vectorOf k gen

randomValue :: Int -> Gen Value
randomValue 1 = oneof 
               [ liftA2 (\x y-> List [Atom "define", x, y]) 
                  randomVarAtom randomNonList
               , liftA2 (\x y -> List [Atom "lambda", x, y]) 
                  (List <$> listOf randomVarAtom) randomNonList
               , liftA3 (\x y z -> List [Atom "if", x, y, z]) 
                    randomNonList randomNonList randomNonList
               , List <$> listOf' randomNonList 
               , randomNonList
               ]
-- randomValue n = oneof 
--                [ liftA2 (\x y-> List [Atom "define", x, y]) 
--                   randomVarAtom (randomValue (n-1))
--                , liftA2 (\x y -> List [Atom "lambda", x, y]) 
--                   (List <$> listOf randomVarAtom) (randomValue (n-1))
--                , liftA3 (\x y z -> List [Atom "if", x, y, z]) 
--                     (randomValue (n-1)) (randomValue (n-1)) (randomValue (n-1))
--                , List <$> listOf (randomValue (n-1)) 
--                , (randomValue (n-1))
--                ]
randomValue n = oneof [List <$> listOf' (randomValue (n-1)), randomValue 1]

instance Arbitrary Value where
  arbitrary = sized randomValue
  -- arbitrary = frequency [ (3, Number <$> arbitrary)
  --                       , (2, pure $ Boolean True)
  --                       , (2, pure $ Boolean False)
  --                       , (5, Atom <$> randomVarAtom)
  --                       , (1, List <$> 
  --                       ]

validTest :: Char -> Bool
validTest x = isPrint x && not (isSpace x) && x /= ')' && x /= '('

validChar :: Parser Char
validChar = satisfy validTest 

atom :: Parser Value
atom = fmap Atom $ some validChar

number :: Parser Value
number = fmap Number $ natural

bool :: Parser Value
bool = fmap Boolean $ 
          (string "#t" *> return True) 
      <|> (string "#f" *> return False)

list :: Parser Value
list = fmap List $ parens $ file

file :: Parser [Value]
file = whiteSpace *> many (   list 
                          <|> token bool
                          <|> number
                          <|> token atom
                          )

getAtoms :: Value -> [String]
getAtoms (Atom x) = pure x
getAtoms (List xs) = nub $ foldMap getAtoms xs
getAtoms _ = mempty

parseFile :: String -> IO (Maybe [Value])
parseFile = parseFromFile file
