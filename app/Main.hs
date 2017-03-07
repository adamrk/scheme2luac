module Main where

import Assembler
import CodeGenerator
import System.Environment(getArgs)

main :: IO ()
main = do
  xs <- getArgs
  let method = if length xs >= 3 && xs !! 2 == "--noOp" then M2 else M1 
  parseAndWrite method (head xs) (head $ tail xs)
  -- f <- compileFromFile (head xs)
  -- case f >>= finalBuilder of 
  --   Just bs -> writeBuilder (xs !! 1) bs
  --   Nothing -> print "error completing builder"
