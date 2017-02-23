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
                                  params=0, vararg=0, maxstack=1,
            instructions = [ IABx  OpClosure 0 0
                           , IABC  OpGetUpVal 0 0 0
                           , IABC  OpCall 0 1 2 
                           , IABC  OpReturn 0 2 0
                           ],
            constants =    [ ],
            functions =    [ lookupEnv inx ]}

genEval t (List [x]) = genEval t x
genEval t (List [Atom "define", Atom x, f]) = 
                    let inx = M.findWithDefault 0 x t
                    in  LuaFunc{ startline=0, endline=0, upvals=1,
                                 params=0, vararg=0, maxstack=3,
            instructions = [ IABC  OpNewTable 0 0 0
                           , IABC  OpGetUpVal 1 0 0
                           , IABC  OpSetTable 0 257 1
                           , IABx  OpClosure 2 0
                           , IABC  OpMove 0 0 0
                           , IABC  OpCall 2 1 2
                           , IABC  OpSetTable 1 256 2
                           , IABC  OpReturn 0 1 0
                           ],
            constants =    [ LuaNumber . fromIntegral $ inx
                           , LuaNumber 0
                           ],
            functions =    [genEval t f]}

genEval t (List [Atom "lambda", List vars, f]) = 
        let nvars = length vars
            inxs = map (\(Atom x) -> M.findWithDefault 0 x t) vars
        in  
                        LuaFunc{ startline=0, endline=0, upvals=1, 
                                 params = fromIntegral nvars, vararg=0, 
                                 maxstack = fromIntegral nvars + 2,
            instructions = [ IABC  OpNewTable nvars 0 0
                           , IABC  OpGetUpVal (nvars+1) 0 0
                           , IABC  OpSetTable nvars (nvars+256) (nvars+1)
                           ] ++
                              (map (\x -> IABC OpSetTable nvars (256+x) x) 
                                [0..nvars-1]) ++
                           [ IABx  OpClosure (nvars+1) 0
                           , IABC  OpMove 0 nvars 0
                           , IABC  OpReturn (nvars+1) 2 0
                           ],

            constants    = map (LuaNumber . fromIntegral) inxs ++ [LuaNumber 0], 
                           
            functions    = [genEval t f]}

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

--genEval t (List [Atom "define", List (Atom f:xs), List vs])

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


lookupOnce :: Int -> LuaFunc
-- takes two params, second is a table
-- we look up index n in the table and if it is nil return (lookup 0, lookup 0)
-- if the value is not nil return (nil, val)
-- The idea is that 0 points to the next environment up so we could call again
lookupOnce n = LuaFunc { startline=0, endline=0, upvals=0, params=2,
                            vararg=0, maxstack=5,
     instructions    = [ IABC  OpLoadNil 2 2 0
                       , IABC  OpGetTable 3 1 256
                       , IABC  OpEq 0 2 3
                       , IAsBx OpJmp 0 3
                       , IABC  OpGetTable 3 1 257
                       , IABC  OpMove 4 3 0
                       , IAsBx OpJmp 0 2
                       , IABC  OpMove 4 3 0
                       , IABC  OpLoadNil 3 3 0
                       , IABC  OpReturn 3 3 0
                       ],
     constants       = [ LuaNumber . fromIntegral $ n
                       , LuaNumber 0 ],
     functions       = []}

lookupEnv :: Int -> LuaFunc
-- assumes table is first upval, looks up the index and traces back to previous
-- environments if null
lookupEnv n =  LuaFunc { startline=0, endline=0, upvals=1, params=0,
                         vararg=0, maxstack=5, -- TForloop returns values in 3,4
        instructions = [ IABx  OpClosure 0 0
                       , IABC  OpLoadNil 1 1 0
                       , IABC  OpGetUpVal 2 0 0
                       , IABC  OpTForLoop 0 0 2
                       , IAsBx OpJmp 0 (-2)
                       , IABC  OpReturn 4 2 0
                       ],
        constants    = [ LuaNumber . fromIntegral $ n
                       , LuaNumber 0
                       ],
        functions    = [lookupOnce n]}


printTable :: Int -> LuaFunc
-- useful for debugging??
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
                   , LuaNumber . fromIntegral $ (n-1)
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

-- getting an error on pulling a nil value from the table

genProgram :: [Value] -> LuaFunc
genProgram vs = let atable = M.fromList $ zip (nub $ foldMap getAtoms vs) [1..] 
                  -- tables zip from 1 because 0 points to prev environment
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
-- Useful for debugging??
addPrintTable vs = let tblSize = (length . nub . foldMap getAtoms $ vs) - 1
                       -- is -1 right?
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

sampleFunc :: LuaFunc -- Example function to test
sampleFunc = LuaFunc {startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack=5, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0
                                , IABC  OpSetTable 0 257 261
                                , IABC  OpSetTable 0 258 262
                                , IABC  OpSetTable 0 259 263
                                , IABC  OpNewTable 1 0 0
                                , IABC  OpSetTable 1 256 0
                                , IABC  OpSetTable 1 257 264
                                , IABC  OpSetTable 1 260 265
                                , IABx  OpClosure 2 0
                                , IABC  OpMove 0 1 0
                                , IABC  OpCall 2 1 2
                                , IABx  OpGetGlobal 3 10
                                , IABC  OpMove 4 2 0
                                , IABC  OpCall 3 2 1
                                , IABC  OpReturn 0 1 0 
                                ], 
                   
                   constants=   [ LuaNumber 0
                                , LuaNumber 1
                                , LuaNumber 2
                                , LuaNumber 3
                                , LuaNumber 4
                                , LuaString "first entry"
                                , LuaString "foo"
                                , LuaString "bar"
                                , LuaString "baz"
                                , LuaString "qux"
                                , LuaString "print"
                                ],
                   
                   functions=   [ lookupEnv 4
                                , printTable 5
                                ]}


luafunc :: IO (Maybe LuaFunc)
luafunc = (fmap . fmap) genProgram $ parseFromFile file "samplecode"

result2maybe :: Result a -> Maybe a
result2maybe (Success x) = Just x
result2maybe (Failure x) = Nothing

experiment :: IO ()
experiment = case finalBuilder sampleFunc' of
                  Just bs -> writeBuilder "temp" bs
                  Nothing -> print "error completing builder"

main :: IO ()
main = do
  f <- luafunc 
  case f >>= finalBuilder of 
    Just bs -> writeBuilder "temp" bs
    Nothing -> print "error completing builder"