module CodeGenerator where

import Assembler
import Parser
import Data.Monoid
import Data.List (foldl')
import Text.Trifecta (parseFromFile, Result(Success, Failure))
import Data.List (nub)
import Data.Maybe (maybeToList)
import qualified Data.Map as M

-- | At compile time we create the `AtomTable` to convert each `Atom` to an 
-- integer. This integer is where the value of the `Atom` will be stored in the 
-- tables that define the environment for each function.
--
type AtomTable = M.Map String Int

-- | Converts a Scheme value into a Lua chunk. The chunk takes no parameters, 
-- but expects a single upvalue which is a table of the defined atoms. Entry 0 
-- in the table points to the table for the environment one level up. The chunk
-- returns a single value which is the result of evaluating the value (or
-- possibly no value e.g. in the case of define).
--
genEval :: AtomTable -> Value -> LuaFunc
genEval _ (Number n) = LuaFunc{ startline=1, endline=1, upvals=1, 
                              params=0, vararg=0, maxstack=1, 
                 instructions=[ 
                                IABx  OpLoadK 0 0 -- Load n
                              , IABC  OpReturn 0 2 0 -- Return n
                              ],   
                 constants=   [ LuaNumber . fromIntegral $ n ], 
                 functions=   []}

genEval _ (Boolean b) = let val = if b then 1 else 0
                        in  LuaFunc{ startline=1, endline=1, upvals=1,
                                     params=0, vararg=0, maxstack=1,
                instructions = [ IABC  OpLoadBool 0 val 0
                               , IABC  OpReturn 0 2 0
                               ],
                constants    = [],
                functions    = []}

genEval t (Atom x) = let inx = M.findWithDefault 0 x t
                     in  LuaFunc{ startline=2, endline=2, upvals=1,
                                  params=0, vararg=0, maxstack=1,
            instructions = [ IABx  OpClosure 0 0 -- closure to look up x
                           , IABC  OpGetUpVal 0 0 0 -- pass env table as upval
                           , IABC  OpCall 0 1 2 -- call lookup closure
                           , IABC  OpReturn 0 2 0 -- return lookup of x
                           ],
            constants =    [ ],
            functions =    [ lookupEnv inx ]}

genEval t (List [Atom "define", Atom x, f]) = 
                    let inx = M.findWithDefault 0 x t
                    in  LuaFunc{ startline=3, endline=3, upvals=1,
                                 params=0, vararg=0, maxstack=3,
            instructions = [ IABC  OpNewTable 0 0 0 -- new env table
                           , IABC  OpGetUpVal 1 0 0 -- old env table to reg 1
                           , IABC  OpSetTable 0 257 1 -- point new env to old
                           , IABx  OpClosure 2 0 -- closure to evaluate f
                           , IABC  OpMove 0 0 0 -- pass new table as env
                           , IABC  OpCall 2 1 2 -- call closure
                           , IABC  OpSetTable 1 256 2 -- set x in old table
                           , IABC  OpReturn 0 1 0 -- return nothing
                           ],
            constants =    [ LuaNumber . fromIntegral $ inx
                           , LuaNumber 0
                           ],
            functions =    [genEval t f]}

genEval t (List [Atom "lambda", List vars, f]) = 
  let nvars = length vars
      inxs  = map (\(Atom x) -> M.findWithDefault 0 x t) vars
  in LuaFunc{ startline=4, endline=4, upvals=1, params=0, vararg=0, 
              maxstack = 1,
    instructions = [ IABx  OpClosure 0 0 -- closure that evaluates f with params
                   , IABC  OpGetUpVal 0 0 0 -- pass env table
                   , IABC  OpReturn 0 2 0 -- return the closure
                   ],
    constants    = [],
    functions    = [genLambda t vars f]}

genEval t (List [Atom "if", cond, exp1, exp2]) = 
    LuaFunc{ startline=4, endline=4, upvals=1, params=0, vararg=0, 
              maxstack = 4,
    instructions = [ IABC  OpGetUpVal 0 0 0 -- get env table
                   , IABC  OpNewTable 1 0 0 --- new env table
                   , IABC  OpSetTable 1 256 0 -- point new table to old
                   , IABx  OpClosure 2 0 -- cond closure
                   , IABC  OpMove 0 1 0 -- pass env
                   , IABC  OpCall 2 1 2 -- call cond
                   , IABC  OpLoadBool 3 1 0 -- load true in reg 3
                   , IABC  OpEq 0 3 2 -- skip if reg 2 is true
                   , IAsBx OpJmp 0 6 -- jump to false case
                   , IABC  OpNewTable 1 0 0 -- new env for exp1
                   , IABC  OpSetTable 1 256 0 -- point new table to old
                   , IABx  OpClosure 2 1 -- exp1 closure
                   , IABC  OpMove 0 1 0 -- pass in env
                   , IABC  OpCall 2 1 2 -- get expr
                   , IAsBx OpJmp 0 5 -- jump to return
                   , IABC  OpNewTable 1 0 0 -- new env for exp2
                   , IABC  OpSetTable 1 256 0 -- point new table to old
                   , IABx  OpClosure 2 2 -- exp2 closure
                   , IABC  OpMove 0 1 0 -- pass in env
                   , IABC  OpCall 2 1 2 -- get expr
                   , IABC  OpReturn 2 2 0
                   ],
    constants    = [ LuaNumber 0 ],
    functions    = [ genEval t cond, genEval t exp1, genEval t exp2 ]}

genEval t (List [Atom "register-global", Atom x]) =
  LuaFunc{ startline=0, endline=0, upvals=1, params=0, vararg=0, maxstack=1,
    instructions = [ IABx  OpClosure 0 0
                   , IABC  OpGetUpVal 0 0 0
                   , IABC  OpCall 0 1 2
                   , IABx  OpSetGlobal 0 0
                   , IABC  OpReturn 0 1 0
                   ],
    constants    = [ LuaString x ],
    functions    = [ genEval t (Atom x) ]}

genEval t (List (f:xs)) = let nvars = length xs
 in 
    LuaFunc { startline=6, endline=6, upvals=1, params=0, vararg=0, 
              maxstack = fromIntegral nvars + 3,
      instructions =   IABC  OpGetUpVal 0 0 0 : -- get env table
                       concatMap (\x -> 
                          [ IABC OpNewTable 1 0 0 -- new env table (reg 1)
                          , IABC OpSetTable 1 256 0 -- point new env to old
                          , IABx OpClosure (x+2) x -- closure for var x 
                          , IABC OpMove 0 1 0 -- pass in new env
                          , IABC OpCall (x+2) 1 2 -- eval x
                          ]) [0..nvars] -- loop over (f=0) then vars
                       ++
                      [ IABC  OpCall 2 (fromIntegral nvars + 1) 2 -- call f
                      , IABC  OpReturn 2 2 0
                      ],
      constants =    [ LuaNumber 0 ],
      functions =    genEval t f : map (genEval t) xs}

-- | `genLambda` takes a list of `Atom`s (as `Value`s) and a `Value` containing
-- those `Atom`s. It creates a chunk which takes a parameter for each value, 
-- binds each parameter to the corresponding `Atom` and the evaluates the final
-- `Value`.
--
genLambda :: AtomTable -> [Value] -> Value -> LuaFunc
genLambda t vars f = 
  let nvars = length vars
      inxs = map (\(Atom x) -> M.findWithDefault 0 x t) vars
  in  
                  LuaFunc{ startline=5, endline=5, upvals=1, 
                           params = fromIntegral nvars, vararg=0, 
                           maxstack = fromIntegral nvars + 2,
      instructions = [ IABC  OpNewTable nvars 0 0 -- new env after params
                     , IABC  OpGetUpVal (nvars+1) 0 0 -- old env
                     , IABC  OpSetTable nvars (nvars+256) (nvars+1)
                         -- ^ point new env to old
                     ] ++
                        (map (\x -> IABC OpSetTable nvars (256+x) x)
                        -- ^ set params in new env 
                          [0..nvars-1]) ++
                     [ IABx  OpClosure (nvars+1) 0 -- closure for f
                     , IABC  OpMove 0 nvars 0 -- pass in new env
                     , IABC  OpCall (nvars+1) 1 2 -- eval f
                     , IABC  OpReturn (nvars+1) 2 0
                     ],
      constants    = map (LuaNumber . fromIntegral) inxs ++ [LuaNumber 0], 
      functions    = [genEval t f]}

-- |Takes two params, second is a table.
-- Looks up index n in the table and if it is nil return (lookup 0, lookup 0)
-- if the value is not nil return (nil, val)
-- The idea is that 0 points to the next environment up so we could call again
--
lookupOnce :: Int -> LuaFunc
lookupOnce n = LuaFunc { startline=0, endline=0, upvals=0, params=2,
                            vararg=0, maxstack=5,
     instructions    = [ IABC  OpLoadNil 2 2 0 -- nil in reg 2
                       , IABC  OpGetTable 3 1 256 -- lookup result -> reg 3
                       , IABC  OpEq 0 2 3 -- if nil result skip next
                       , IAsBx OpJmp 0 3 -- Jump 3
                       , IABC  OpGetTable 3 1 257 -- pointer to up env -> reg 3
                       , IABC  OpMove 4 3 0 -- copy pointer to reg 4
                       , IAsBx OpJmp 0 2 -- jump to return 
                       , IABC  OpMove 4 3 0 -- copy val to reg 4
                       , IABC  OpLoadNil 3 3 0 -- load nill in reg 3
                       , IABC  OpReturn 3 3 0 -- return reg 3 and 4
                       ],
     constants       = [ LuaNumber . fromIntegral $ n
                       , LuaNumber 0 ],
     functions       = []}

-- |Assumes table is first upval, looks up the index and traces back to previous
-- environments if nil. Returns nil if not present in any table. Should this be
-- changed to lookup in global env if still nil?
--
lookupEnv :: Int -> LuaFunc
lookupEnv n =  LuaFunc { startline=0, endline=0, upvals=1, params=0,
                         vararg=0, maxstack=5, -- TForloop returns values in 3,4
        instructions = [ IABx  OpClosure 0 0 -- lookup once closure (iter func)
                       , IABC  OpLoadNil 1 1 0 -- state (nil when done)
                       , IABC  OpGetUpVal 2 0 0 -- env table -> reg 2
                       , IABC  OpTForLoop 0 0 2 -- calls the lookup closure
                        -- ^ return vals stored in 3,4 then copy 3->2 if done,
                        -- ^ otherwise skip next
                       , IAsBx OpJmp 0 (-2) -- back to loop
                       , IABC  OpReturn 4 2 0 -- return register 4
                       ],
        constants    = [ LuaNumber . fromIntegral $ n
                       , LuaNumber 0
                       ],
        functions    = [lookupOnce n]}

-- |put the closure at index n in register 2, pass register 0 as upval and call 
-- it without inputs
--
closeAndCall :: Int -> [LuaInstruction]
closeAndCall n = [ IABx  OpClosure 2 n
                 , IABC  OpMove 0 0 0 
                 , IABC  OpCall 2 1 2
                 ]

-- | Generate the Lua chunk which evaluates each value and prints the final one
--
genProgram :: [Value] -> LuaFunc
genProgram vs = let atable = M.fromList $ zip (nub $ foldMap getAtoms vs) [1..] 
                  -- ^tables zip from 1 because 0 points to prev environment
                    funcs = addPrim atable : map (genEval atable) vs
                    ins =   IABC OpNewTable 0 0 0 -- original env 
                          : IABx OpGetGlobal 1 0 -- print -> reg 1
                          : foldMap closeAndCall [0..length vs] -- eval vals 
                          ++
                          [ IABC OpCall 1 2 1 -- call print on reg 2
                          , IABC OpReturn 0 1 0
                          ]
                    cns = [ LuaString "print" ]
                in  LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                             vararg=2, maxstack=3,instructions=ins, 
                             constants=cns, functions=funcs}

------------------- Functions to load at beginning ------------------------

primitives :: [(String, LuaFunc)]
primitives = [ ("*", LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                              vararg=2, maxstack=7, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0 -- to hold values
                                , IABC  OpVarArg 1 0 0 -- args in reg 1 and up
                                , IABC  OpSetList 0 0 1 -- save args to table
                                , IABx  OpLoadK 1 0 -- load 1 (loop init)
                                , IABC  OpLen 2 0 0 -- load length (loop max)
                                , IABx  OpLoadK 3 0 -- load 1 (loop step)
                                , IABx  OpLoadK 5 0 -- first arg
                                , IAsBx OpForPrep 1 2 
                                , IABC  OpGetTable 6 0 1 -- next arg
                                , IABC  OpMul 5 5 6 -- product -> r5
                                , IAsBx OpForLoop 1 (-3)
                                , IABC  OpReturn 5 2 0 -- return 
                                ], 
                   constants=   [ LuaNumber 1
                                ],
                   functions=   []}) 
             , ("+", LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                              vararg=2, maxstack=7, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0
                                , IABC  OpVarArg 1 0 0
                                , IABC  OpSetList 0 0 1
                                , IABx  OpLoadK 1 1
                                , IABC  OpLen 2 0 0
                                , IABx  OpLoadK 3 1
                                , IABx  OpLoadK 5 0
                                , IAsBx OpForPrep 1 2
                                , IABC  OpGetTable 6 0 1
                                , IABC  OpAdd 5 5 6
                                , IAsBx OpForLoop 1 (-3)
                                , IABC  OpReturn 5 2 0
                                ], 
                   
                   constants=   [ LuaNumber 0
                                , LuaNumber 1
                                ],
                   
                   functions=   []})
             , ("-", LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                              vararg=2, maxstack=9, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0 -- table for args
                                , IABC  OpVarArg 1 0 0 -- load arguments
                                , IABC  OpSetList 0 0 1  -- save args in table
                                , IABx  OpLoadK 1 2 -- 2 -> reg 1 (loop init)
                                , IABC  OpLen 2 0 0 -- #args -> reg 2 (loop max) 
                                , IABx  OpLoadK 3 1 -- 1 -> reg 3 (loop step)
                                , IABC  OpGetTable 5 0 257 -- first arg -> reg 5
                                , IABC  OpEq 0 3 2 -- if #args = 1 pc+
                                , IAsBx OpJmp 0 2 -- jump to for loop
                                , IABC  OpUnM 5 5 0 -- negate first arg
                                , IAsBx OpJmp 0 4 -- jump to return
                                , IAsBx OpForPrep 1 2 
                                , IABC  OpGetTable 6 0 1 -- load table value
                                , IABC  OpSub 5 5 6 -- r5 = r5 - r6
                                , IAsBx OpForLoop 1 (-3)  
                                , IABC  OpReturn 5 2 0
                                ], 
                   
                   constants=   [ LuaNumber 0
                                , LuaNumber 1
                                , LuaNumber 2
                                ],
                   
                   functions=   []})
             , ("quotient", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpDiv 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("modulo", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpMod 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("expt", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpPow 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("not", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=1, vararg=0, maxstack = 1,
                    instructions = [ IABC  OpNot 0 0 0 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("=", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpEq 0 0 1 -- if eq then PC++
                                   , IAsBx OpJmp 0 1 -- jmp requred after eq
                                   , IABC  OpLoadBool 0 1 1 -- load true, PC++
                                   , IABC  OpLoadBool 0 0 0 -- load false  
                                   , IABC  OpReturn 0 2 0 
                                   ],
                    constants    = [],
                    functions    = []})
             , ("<", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpLT 0 0 1 -- if lt then PC++
                                   , IAsBx OpJmp 0 1 -- jmp requred after lt
                                   , IABC  OpLoadBool 0 1 1 -- load true, PC++
                                   , IABC  OpLoadBool 0 0 0 -- load false  
                                   , IABC  OpReturn 0 2 0 
                                   ],
                    constants    = [],
                    functions    = []})
             , (">", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                    instructions = [ IABC  OpLT 0 1 0 -- if gt then PC++
                                   , IAsBx OpJmp 0 1 -- jmp requred after gt
                                   , IABC  OpLoadBool 0 1 1 -- load true, PC++
                                   , IABC  OpLoadBool 0 0 0 -- load false  
                                   , IABC  OpReturn 0 2 0 
                                   ],
                    constants    = [],
                    functions    = []})
             ]

addPrim :: AtomTable -> LuaFunc
addPrim t = 
  let inxfunc = foldMap (\(x,f) -> maybeToList ((,) f <$> M.lookup x t)) 
                  $ primitives -- table inx, function tuples
      funcs = map fst inxfunc -- list of funcs
      inxs = map (LuaNumber . fromIntegral . snd) inxfunc -- list of indexes
      np = length funcs
  in  
    LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, maxstack=2,
      instructions = [ IABC  OpGetUpVal 0 0 0
                     ]
                     ++ (foldMap (\n -> [ IABx  OpClosure 1 n
                                       , IABC  OpSetTable 0 (256+n) 1 ]) 
                                [0..np-1])
                     ++
                     [ IABC  OpReturn 0 1 0 
                     ],
      constants    = inxs,
      functions    = funcs}

------------------------- Main IO -----------------------------

luafunc :: IO (Maybe LuaFunc)
luafunc = (fmap . fmap) genProgram $ parseFromFile file "samplecode"

compileFromFile :: String -> IO (Maybe LuaFunc)
compileFromFile = (fmap . fmap) genProgram . parseFromFile file

result2maybe :: Result a -> Maybe a
result2maybe (Success x) = Just x
result2maybe (Failure x) = Nothing

------------------------- Probably not for production ------------------------


-- | Useful for debugging??? Should probably rewrite without the VM loop since
-- we know the length beforehand.
--
copyTable :: Int -> LuaFunc
copyTable n = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
                        maxstack=7,
    instructions = [ IABC  OpGetUpVal 0 0 0 -- get env table (reg 0)
                   , IABC  OpNewTable 1 0 0 -- new table (reg 1)
                   , IABx  OpLoadK 2 0 -- loop init 0
                   , IABx  OpLoadK 3 1 -- loop max 
                   , IABx  OpLoadK 4 2 -- loop step
                   , IAsBx OpForPrep 2 2 
                   -- skip register 5 for external loop variable
                   , IABC  OpGetTable 6 0 2 -- copy from old to reg 6 
                   , IABC  OpSetTable 1 2 6 -- copy from reg 6 to new table
                   , IAsBx OpForLoop 2 (-3) -- loop back to GetTable
                   , IABC  OpReturn 1 2 0
                   ],
    constants    = [ LuaNumber 0
                   , LuaNumber . fromIntegral $ (n-2)
                   , LuaNumber 1
                   ],
    functions    = []}

-- |useful for debugging??
printTable :: Int -> LuaFunc
printTable n = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0,
                         maxstack=7,
    instructions = [ IABC  OpGetUpVal 0 0 0 -- get env table
                   , IABx  OpLoadK 1 0 -- loop init 0
                   , IABx  OpLoadK 2 1 -- loop max n-1
                   , IABx  OpLoadK 3 2 -- loop step 1
                   , IAsBx OpForPrep 1 3 
                   , IABx  OpGetGlobal 5 3 -- print closure -> reg 5
                   , IABC  OpGetTable 6 0 1 -- table val -> reg 6
                   , IABC  OpCall 5 2 1 -- call print
                   , IAsBx OpForLoop 1 (-4)
                   , IABC  OpReturn 0 1 0
                   ],
    constants    = [ LuaNumber 0
                   , LuaNumber . fromIntegral $ (n-1)
                   , LuaNumber 1
                   , LuaString "print"
                   ],
    functions    = []}

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

sumFunc :: LuaFunc -- Example function to test
sumFunc = LuaFunc {startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack=7, 
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0
                                , IABC  OpVarArg 1 0 0
                                , IABC  OpSetList 0 0 1
                                , IABx  OpLoadK 1 1
                                , IABC  OpLen 2 0 0
                                , IABx  OpLoadK 3 1
                                , IABx  OpLoadK 5 0
                                , IAsBx OpForPrep 1 2
                                , IABC  OpGetTable 6 0 1
                                , IABC  OpAdd 5 5 6
                                , IAsBx OpForLoop 1 (-3)
                                , IABC  OpReturn 5 2 0
                                ], 
                   
                   constants=   [ LuaNumber 0
                                , LuaNumber 1
                                ],
                   
                   functions=   [ 
                                ]}

sampleFunc :: LuaFunc -- Example function to test
sampleFunc = LuaFunc {startline=0, endline=0, upvals=0, params=0, vararg=2,
                   maxstack=6, 
                   instructions=[ IABx OpGetGlobal 0 0
                                , IABx OpClosure 1 0
                                , IABx OpLoadK 2 1
                                , IABx OpLoadK 3 2
                                , IABx OpLoadK 4 3
                                , IABx OpLoadK 5 4
                                , IABC OpCall 1 0 2
                                , IABC OpCall 0 2 1
                                --, IABC OpSetList 0 0 1
                                --, IABx  OpGetGlobal 0 0
                                --, IABx  OpClosure 1 0
                                -- , IABx  OpLoadK 2 1
                                -- , IABx  OpLoadK 3 2
                                -- , IABx  OpLoadK 4 3
                                -- , IABC  OpCall 1 4 2 
                                -- , IABC  OpCall 0 2 1  
                                , IABC  OpReturn 0 1 0 
                                ], 
                   
                   constants=   [ 
                                  LuaString "print"
                                , LuaNumber 0
                                , LuaNumber 1
                                , LuaNumber 2
                                , LuaNumber 3
                                , LuaNumber 4
                                ],
                   
                   functions=   [ sumFunc 
                                ]}

experiment :: IO ()
experiment = case finalBuilder sampleFunc of
                  Just bs -> writeBuilder "temp" bs
                  Nothing -> print "error completing builder"
