module Main where

import Assembler
import CodeGenerator
import System.Environment(getArgs)

main :: IO ()
main = do
  xs <- getArgs
  f <- compileFromFile (head xs)
  case f >>= finalBuilder of 
    Just bs -> writeBuilder (xs !! 1) bs
    Nothing -> print "error completing builder"
