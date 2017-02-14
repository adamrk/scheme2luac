module Main where

import AST
import Assembler
import Data.Monoid
import Data.List (foldl')

type BasicLuaFunc = ([LuaInstruction], [LuaConst], Int) -- int is top of stack

genInst :: BasicLuaFunc -> Inst -> BasicLuaFunc
genInst (ins, cns, n) (OpLoad' x) = ( ins ++ [IABx OpLoadK (n+1) (length cns)]
                                    , cns ++ [LuaNumber $ fromIntegral x]
                                    , n + 1
                                    )
genInst (ins, cns, n) OpAdd' = ( ins ++ [IABC OpAdd (n-1) (n-1) n]
                               , cns
                               , n - 1
                               )
genInst (ins, cns, n) OpMult' = ( ins ++ [IABC OpMul (n-1) (n-1) n]
                                , cns
                                , n - 1
                                )
genInst (ins, cns, n) OpSubr' = ( ins ++ [IABC OpSub (n-1) (n-1) n]
                                , cns
                                , n - 1)

stackInc :: Inst -> Sum Int
stackInc (OpLoad' _) = Sum 1
stackInc _ = mempty

getMaxStack :: [Inst] -> Sum Int
getMaxStack = foldMap stackInc

startIns :: BasicLuaFunc
startIns = ([IABx OpGetGlobal 0 0], [LuaString "print"], 0)

foldIns :: [Inst] -> BasicLuaFunc
foldIns = foldl' genInst startIns

genFunc :: [Inst] -> LuaFunc
genFunc xs = let (ins, cns, n) = foldIns xs
                 Sum maxStack = getMaxStack xs
             in  LuaFunc {startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack= fromIntegral maxStack + 1, 
                   instructions= ins ++ [IABC OpCall 0 2 1, IABC OpReturn 0 1 0], 
                   constants=cns,
                   functions=[]}

foofunc :: LuaFunc
foofunc = genFunc . inst $ foo'

main :: IO ()
main = case finalBuilder foofunc of 
  Just bs -> writeBuilder "temp" bs
  Nothing -> print "error completing builder"