module Main where

import Assembler
import CodeGenerator
import System.Environment(getArgs)

main :: IO ()
main = do
  xs <- getArgs
  if length xs /= 2 
    then putStrLn "Usage: scheme2luac-exe INPUT_FILE OUTPUT_FILE"
    else parseAndWrite (head xs) (head $ tail xs)
