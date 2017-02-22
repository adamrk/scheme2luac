module Main where

import AST
import Assembler
import Parser
import Data.Monoid
import Data.List (foldl')
import Text.Trifecta (parseFromFile, Result(Success, Failure))
import Data.List (nub)
import qualified Data.Map as M

type AtomTable = M.Map String Int

genEval :: AtomTable -> Value -> LuaFunc
genEval _ (Number n) = LuaFunc{ startline=0, endline=0, upvals=1, 
                              params=0, vararg=0, maxstack=1, 
                 instructions=[ 
                                IABx  OpLoadK 0 0
                              , IABC  OpReturn 0 2 0
                              ],   
                 constants=   [ LuaNumber . fromIntegral $ n ], 
                 functions=   []}

genEval t (Atom x) = let inx = M.findWithDefault 0 x t
                     in  LuaFunc{ startline=0, endline=0, upvals=1,
                                  params=0, vararg=0, maxstack=2,
            instructions = [ IABC  OpGetUpVal 0 0 0
                           , IABC  OpGetTable 1 0 256
                           , IABC  OpReturn 1 2 0
                           ],
            constants =    [LuaNumber . fromIntegral $ inx],
            functions =    []}

genEval t (List [x]) = genEval t x
genEval t (List [Atom "define", Atom x, f]) = 
                    let inx = M.findWithDefault 0 x t
                    in  LuaFunc{ startline=0, endline=0, upvals=1,
                                 params=0, vararg=0, maxstack=2,
            instructions = [ IABC  OpGetUpVal 0 0 0
                           , IABx  OpClosure 1 0
                           , IABC  OpGetUpVal 0 0 0
                           , IABC  OpCall 1 1 2
                           , IABC  OpSetTable 0 256 1
                           , IABC  OpReturn 0 1 0
                           ],
            constants =    [ LuaNumber . fromIntegral $ inx
                           ],
            functions =    [genEval t f]}

genEval t (List [Atom "+", x, y]) = 
    LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, maxstack=2,
      instructions = [ IABx  OpClosure 0 0
                     , IABC  OpGetUpVal 0 0 0
                     , IABC  OpCall 0 1 2
                     , IABx  OpClosure 1 1
                     , IABC  OpGetUpVal 0 0 0
                     , IABC  OpCall 1 1 2
                     , IABC  OpAdd 0 0 1
                     , IABC  OpReturn 0 2 0
                     ],
      constants = [],
      functions = [genEval t x, genEval t y]}

--genEval t (List [Atom ])

copyTable :: Int -> LuaFunc
copyTable n = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
                        maxstack=7,
    instructions = [ IABC  OpGetUpVal 0 0 0
                   , IABC  OpNewTable 1 0 0 
                   , IABx  OpLoadK 2 0
                   , IABx  OpLoadK 3 1
                   , IABx  OpLoadK 4 2
                   , IAsBx OpForPrep 2 2
                   -- skip register 5 for external loop variable
                   , IABC  OpGetTable 6 0 2 
                   , IABC  OpSetTable 1 2 6
                   , IAsBx OpForLoop 2 (-3)
                   , IABC  OpReturn 1 2 0
                   ], -- Working here!!!!!!!!!!!!!!!!!1
    constants    = [ LuaNumber 0
                   , LuaNumber . fromIntegral $ (n-2)
                   , LuaNumber 1
                   ],
    functions    = []}

printTable :: Int -> LuaFunc
printTable n = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0,
                         maxstack=7,
    instructions = [ IABC  OpGetUpVal 0 0 0
                   , IABx  OpLoadK 1 0
                   , IABx  OpLoadK 2 1
                   , IABx  OpLoadK 3 2
                   , IAsBx OpForPrep 1 3
                   , IABx  OpGetGlobal 5 3
                   , IABC  OpGetTable 6 0 1
                   , IABC  OpCall 5 2 1
                   , IAsBx OpForLoop 1 (-4)
                   , IABC  OpReturn 0 1 0
                   ],
    constants    = [ LuaNumber 0
                   , LuaNumber . fromIntegral $ (n-2)
                   , LuaNumber 1
                   , LuaString "print"
                   ],
    functions    = []}

closeAndCall :: Int -> [LuaInstruction]
-- put the closure in register 2 and call it without inputs
closeAndCall n = [ IABx  OpClosure 2 n
                 , IABC  OpMove 0 0 0 
                 , IABC  OpCall 2 1 2
                 ]

genProgram :: [Value] -> LuaFunc
genProgram vs = let atable = M.fromList $ zip (nub $ foldMap getAtoms vs) [0..] 
                    funcs = map (genEval atable) vs
                    ins = IABC OpNewTable 0 0 0 : IABx OpGetGlobal 1 0 : 
                          foldMap closeAndCall [0..length vs - 1] ++
                          [ IABC OpCall 1 2 1
                          , IABC OpReturn 0 1 0]
                    cns = [LuaString "print"]
                in  LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                             vararg=2, maxstack=3,instructions=ins, 
                             constants=cns, functions=funcs}

addPrintTable :: [Value] -> LuaFunc
addPrintTable vs = let tblSize = length $ zip (nub $ foldMap getAtoms vs) [0..]
                       nFuncs = length vs
                       func = genProgram vs
                   in  LuaFunc { startline = startline func
                               , endline = endline func
                               , upvals = 0, params = 0, vararg = 2
                               , maxstack = 3
                               , instructions = init (instructions func) ++
                                 [ IABx OpClosure 2 nFuncs
                                 , IABC OpMove 0 0 0
                                 , IABC OpCall 2 1 1
                                 , IABC OpReturn 0 1 0
                                 ]
                               , constants = constants func
                               , functions = functions func ++ 
                                    [printTable tblSize]
                               }


luafunc :: IO (Maybe LuaFunc)
luafunc = (fmap . fmap) addPrintTable $ parseFromFile file "samplecode"

result2maybe :: Result a -> Maybe a
result2maybe (Success x) = Just x
result2maybe (Failure x) = Nothing
















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
main = do
  f <- luafunc 
  case f >>= finalBuilder of 
    Just bs -> writeBuilder "temp" bs
    Nothing -> print "error completing builder"