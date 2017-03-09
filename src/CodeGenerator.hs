module CodeGenerator where

import Assembler
import Parser
import Parser2
import Data.Monoid
import Data.List (foldl', elemIndex)
import Text.Trifecta (parseFromFile, Result(Success, Failure), parseString)
import Data.List (nub)
import Data.Maybe (maybeToList, isJust)
import qualified Data.Map as M
import qualified Data.Set as S

data PartialLuaFunc = PartialLuaFunc  { inst :: [LuaInstruction]
                                      , cnst :: [LuaConst]
                                      , funcs :: [LuaFunc]
                                      } deriving (Eq, Show)

emptyPartialFunc = PartialLuaFunc [] [] []

-- | Append instructions, constants, and functions to the partial func to place
-- the evaluated expr in the register indicated by the int
--
addExpr :: PartialLuaFunc -> Int -> AnnExpr -> PartialLuaFunc
addExpr f n (Var s t) = case nScopes of 
  Just k -> PartialLuaFunc
    { inst = inst f ++ [ IABx OpGetGlobal n cinx
                       , IAsBx OpJmp 0 0
                       , IABC OpGetUpVal (n+1) 0 0 
                       , IABx OpLoadK (n+2) (cinx + 1)
                       , IABx OpLoadK (n+3) (cinx + 2)
                       , IABC OpCall n 4 2
                       ]
    , cnst = cnst f ++ [ LuaString "var_lookup"
                       , LuaString s
                       , LuaNumber . fromIntegral $ k
                       ]
    , funcs = funcs f
    }
  Nothing -> PartialLuaFunc
    { inst = inst f ++ [ IABx OpGetGlobal n cinx ]
    , cnst = cnst f ++ [ LuaString s ]
    , funcs = funcs f
    }
  where
    nScopes = M.lookup s t
    cinx = length $ cnst f

addExpr f n (Literal (LitBool b)) = PartialLuaFunc
  { inst = inst f ++ [ IABC OpLoadBool n val 0 ]
  , cnst = cnst f
  , funcs = funcs f
  } 
  where
    val = if b then 1 else 0

addExpr f n (Literal cs) = PartialLuaFunc
  { inst = inst f ++ [ IABx OpLoadK n inx ]
  , cnst = newcnst
  , funcs = funcs f
  }
  where
    val = case cs of
      LitChar c -> LuaString [c]
      LitStr s -> LuaString s
      LitNum m -> LuaNumber m
    (inx, newcnst) = case elemIndex val (cnst f) of
      Just m -> (m, cnst f)
      Nothing -> (length $ cnst f, cnst f ++ [val])

addExpr f n c@(Call _ _) = PartialLuaFunc
  { inst = inst f ++ [ IABx OpClosure n inx
                     , IABC OpGetUpVal 0 0 0
                     , IABC OpCall n 1 2
                     ]
  , cnst = cnst f
  , funcs = funcs f ++ [toFunc c] 
  }
  where
    inx = length . funcs $ f

addExpr f n (Lambda vs b) = PartialLuaFunc
  { inst = inst f ++ [ IABx OpClosure n inx
                     , IABC OpGetUpVal 0 0 0
                     ]
  , cnst = cnst f
  , funcs = funcs f ++ [toFuncLambda vs b]
  }
  where 
    inx = length . funcs $ f

addExpr f n c@(Cond _ _ _) = PartialLuaFunc
  { inst = inst f ++ [ IABx OpClosure n inx
                     , IABC OpGetUpVal 0 0 0
                     , IABC OpCall n 1 2
                     ]
  , cnst = cnst f
  , funcs = funcs f ++ [toFunc c]
  }
  where
    inx = length . funcs $ f

completeFunc :: String -> PartialLuaFunc -> LuaFunc
completeFunc s f = LuaFunc { startline=0, endline=0, upvals=1, params=0,
                             vararg=0, source=s, instructions=inst f,
                             constants=cnst f, functions=funcs f,
                             maxstack = fromIntegral . (+1) . maximum $ 
                              map maxReg (inst f)
                           }

-- | Lua chunk that takes 3 parameters: environment table, variable string, and 
-- the number of environments to go up. It returns the value at the string in 
-- that environment.
--
luaLookup :: LuaFunc
luaLookup = LuaFunc { startline=0, endline=0, upvals=0, params=3, vararg=0, 
                      maxstack=8, source="@lookup\0",
    instructions =    [ IABx  OpLoadK 3 1 -- 1 for init
                      , IABC  OpMove 4 2 0 -- 3rd param for limit
                      , IABx  OpLoadK 5 1 -- 1 for step
                      , IABC  OpMove 7 0 0 -- env table from 1st param
                      , IAsBx OpForPrep 3 1 
                      , IABC  OpGetTable 7 7 256 -- get next environment
                      , IAsBx OpForLoop 3 (-2)
                      , IABC  OpGetTable 0 7 1 -- lookup 2nd param
                      , IABC  OpReturn 0 2 0
                      ],
    constants =       [ LuaNumber 0
                      , LuaNumber 1
                      ],
    functions =       []
                    } 

-- | Converts an annotated scheme expression into a Lua chunk. The chunk takes 
-- no parameters and a single upval which is the evaluation environment. When 
-- called the chunk will return the value that the expression evaluates to.
--
toFunc :: AnnExpr -> LuaFunc
toFunc (Var s t) 
      -- | inx tell us how many scopes out to look for s
  | isJust inx = let Just n = inx
                 in  LuaFunc { startline=0, endline=0, upvals=1, params=0,
                              vararg=0, maxstack=4, source="@var" ++ s ++ "\0",
        instructions =        [ IABx  OpGetGlobal 0 2 -- lookup closure
                              , IAsBx OpJmp 0 0
                              , IABC  OpGetUpVal 1 0 0 -- get env env
                              , IABx  OpLoadK 2 0 -- load the variable name
                              , IABx  OpLoadK 3 1 -- load # envs up
                              , IABC  OpTailCall 0 4 0
                              , IABC  OpReturn 0 0 0
                              ],
        constants =           [ LuaString s 
                              , LuaNumber . fromIntegral $ n
                              , LuaString "var_lookup"
                              ],
        functions =           [] }
  | otherwise = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0,
    -- ^ if there is no indicated scope, assume it is a global func
                          maxstack=1, source="@glvar" ++ s ++ "\0",
        instructions =    [ IABx  OpGetGlobal 0 0
                          , IABC  OpReturn 0 2 0
                          ],
        constants =       [ LuaString s ],
        functions =       []}
  where
    inx = M.lookup s t

toFunc (Literal (LitBool b)) =
  LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
            maxstack=1, source="@bool\0",
    instructions = [ IABC  OpLoadBool 0 (if b then 1 else 0) 0
                   , IABC  OpReturn 0 2 0
                   ],
    constants =    [],
    functions =    []}

toFunc (Literal (LitNum n)) = 
  LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
            maxstack=1, source="@num" ++ show n ++ "\0",
    instructions = [ IABx  OpLoadK 0 0
                   , IABC  OpReturn 0 2 0
                   ],
    constants =    [ LuaNumber n ],
    functions =    []}

toFunc (Literal (LitChar c)) = 
  LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
            maxstack=1, source="@char" ++ show c ++ "\0",
    instructions = [ IABx  OpLoadK 0 0
                   , IABC  OpReturn 0 2 0
                   ],
    constants =    [ LuaString [c] ],
    functions =    []}

toFunc (Literal (LitStr s)) = 
  LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
            maxstack=1, source="@str" ++ show s ++ "\0",
    instructions = [ IABx  OpLoadK 0 0
                   , IABC  OpReturn 0 2 0
                   ],
    constants =    [ LuaString s ],
    functions =    []}

toFunc (Call f xs) = completeFunc "@call\0" $ PartialLuaFunc
  { inst = inst placedVals ++ [ IABC OpTailCall 0 (fromIntegral nvars + 1) 0
                              , IABC OpReturn 0 0 0
                              ]
  , cnst = cnst placedVals
  , funcs = funcs placedVals
  }
  where
    foldhelp p (n, e) = addExpr p n e
    placedVals = foldl' foldhelp emptyPartialFunc $ zip [0..] (f:xs)
    nvars = length xs

-- toFunc (Call f xs) = let nvars = length xs
--  in 
--     LuaFunc { startline=6, endline=6, upvals=1, params=0, vararg=0, 
--               maxstack = fromIntegral nvars + 1, source="@call\0",
--       instructions =  concatMap (\x -> 
--                           [ IABx OpClosure x x -- closure for var x 
--                           , IABC OpGetUpVal 0 0 0 -- pass in new env
--                           , IABC OpCall x 1 2 -- eval x
--                           ]) [0..nvars] -- loop over (f=0) then vars
--                        ++
--                       [ IABC  OpTailCall 0 (fromIntegral nvars + 1) 0 -- call f
--                       , IABC  OpReturn 0 0 0
--                       ],
--       constants =    [],
--       functions =    toFunc f : map toFunc xs}

toFunc (Lambda vs b) = LuaFunc{ startline=4, endline=4, upvals=1, 
                                 params=0, vararg=0, maxstack = 1, 
                                 source="@lambda\0",
    instructions = [ IABx  OpClosure 0 0 -- closure that evals f with params
                   , IABC  OpGetUpVal 0 0 0 -- pass env table
                   , IABC  OpReturn 0 2 0 -- return the closure
                   ],
    constants    = [],
    functions    = [toFuncLambda vs b]}

toFunc (Cond a b c) = LuaFunc{ startline=4, endline=4, upvals=1, params=0, 
                                vararg=0, maxstack = 4, source="@if\0",
    instructions = [ IABC  OpGetUpVal 0 0 0 -- get env table
                   , IABx  OpClosure 1 0 -- cond closure
                   , IABC  OpMove 0 0 0 -- pass env
                   , IABC  OpCall 1 1 2 -- call cond
                   , IABC  OpLoadBool 2 1 0 -- load true in reg 3
                   , IABC  OpEq 0 2 1 -- skip if reg 2 is true
                   , IAsBx OpJmp 0 3 -- jump to false case
                   , IABx  OpClosure 1 1 -- exp1 closure
                   , IABC  OpMove 0 0 0 -- pass in env
                   -- , IABC  OpTailCall 1 1 0 -- get expr
                   -- , IABC  OpReturn 1 0 0
                   , IAsBx OpJmp 0 2 -- jump to return
                   , IABx  OpClosure 1 2 -- exp2 closure
                   , IABC  OpMove 0 0 0 -- pass in env
                   , IABC  OpTailCall 1 1 0 -- get expr
                   , IABC  OpReturn 1 0 0
                   ],
    constants    = [ LuaNumber 0 ],
    functions    = [ toFunc a, toFunc b, toFunc c ]}

toFunc (Assign _ _) = undefined
-- ^ Still need to do this!!!!!!!

-- |Turn a lambda into a lua chunk. The funcion takes one parameter for each 
-- variable, binds them to the corresponding names in the environment and then
--   evaluates the body.
--
toFuncLambda :: [Expr] -> AnnBody -> LuaFunc
toFuncLambda vars f = let nvars = length vars
  in  
                  LuaFunc{ startline=5, endline=5, upvals=1, 
                           params = fromIntegral nvars, vararg=0, 
                           maxstack = fromIntegral nvars + 2,
                           source="@lambdabody\0",
      instructions = [ IABC  OpNewTable nvars 0 0 -- new env after params
                     , IABC  OpGetUpVal (nvars+1) 0 0 -- old env
                     , IABC  OpSetTable nvars (nvars+256) (nvars+1) 
                         -- point new env to old
                     ] ++
                        (map (\x -> IABC OpSetTable nvars (256+x) x)
                        -- ^ set params in new env 
                          [0..nvars-1]) ++
                     [ IABx  OpClosure (nvars+1) 0 -- closure for f
                     , IABC  OpMove 0 nvars 0 -- pass in new env
                     , IABC  OpTailCall (nvars+1) 1 0 -- eval f
                     , IABC  OpReturn (nvars+1) 0 0
                     ],
      constants    = map (\(Var x _) -> LuaString x) vars ++ [LuaNumber 0], 
      functions    = [toFuncBody f]}

-- |The lua chunk that evaluates a body simply evaluates each def or expr in
-- turn and then returns the last one.
--
toFuncBody :: AnnBody -> LuaFunc
toFuncBody (Body ds es) = LuaFunc{ startline=0, endline=0, upvals=1, params=0,
                                    vararg=0, maxstack=1, source="@inBody\0",
            instructions = concatMap (\i -> 
                             [ IABx  OpClosure 0 i -- get def or expr
                             , IABC  OpGetUpVal 0 0 0 -- pass env
                             , IABC  OpCall 0 1 2
                             ]) [0.. n - 2]
                          ++ [ IABx  OpClosure 0 (n - 1)
                             , IABC  OpGetUpVal 0 0 0
                             , IABC  OpTailCall 0 0 0
                             , IABC  OpReturn 0 0 0
                             ],
            constants    = [],
            functions    = map toFuncDef ds ++ map toFunc es}
  where
    n = length ds + length es
-- |Turn a def into a lua chunk by evaluating the expression and then binding it
-- to the proper variable name in the current environment.
-- 
toFuncDef :: AnnDef -> LuaFunc
toFuncDef (Def1 (Var x _) e) = LuaFunc{ startline=3, endline=3, upvals=1,
                                 params=0, vararg=0, maxstack=2, 
                                 source="@def1" ++ show x ++ "\0",
            instructions = [ IABC  OpGetUpVal 0 0 0 -- get env
                           , IABx  OpClosure 1 0 -- closure to evaluate e
                           , IABC  OpMove 0 0 0 -- pass env
                           , IABC  OpCall 1 1 2 -- call closure
                           , IABC  OpSetTable 0 256 1 -- set x in old table
                           , IABC  OpReturn 0 1 0 -- return nothing
                           ],
            constants =    [ LuaString x ],
            functions =    [ toFunc e ]}
toFuncDef (Def2 x vs b) = toFuncDef $ Def1 x (Lambda vs b)
toFuncDef (Def3 ds) = LuaFunc{ startline=0, endline=0, upvals=1,
                                params=0, vararg=0, maxstack=1, 
                                source="@def3\0",
          instructions = concatMap (\n -> 
                            [ IABx OpClosure 0 n -- get def closure
                            , IABC OpGetUpVal 0 0 0 -- pass env
                            , IABC OpCall 0 1 1 -- call closure
                            ]) [0..length ds - 1]
                         ++ [ IABC OpReturn 0 1 0 ],
          constants =    [],
          functions =    map toFuncDef ds} 

toFuncProgram :: [CommOrDef] -> LuaFunc
toFuncProgram xs = LuaFunc{ startline=0, endline=0, upvals=0, params=0, 
                            vararg=0, maxstack=3, source = "@main\0",
        instructions = [ IABC OpNewTable 0 0 0 -- env table
                       , IABx OpGetGlobal 1 0 -- print command
                       ] 
                       ++ concatMap regGlobal [0..length globals - 1] 
                       ++ concatMap createAndCall [0..length xs - 1] ++
                       [ IABC OpCall 1 2 1
                       , IABC OpReturn 0 1 0
                       ],
        constants =    [ LuaString "print" ] ++ map (LuaString . fst) globals,
        functions =    map tofunc axs ++ map snd globals 
      }
  where
    axs = annotateProgram (M.fromList []) xs -- annotate the parse tree
    freeVars = allVars xs -- search for free variables
    globals = ("var_lookup", luaLookup) 
      : filter ((`S.member` freeVars) . fst) primitives -- add needed prims
    tofunc (Comm x) = toFunc x
    tofunc (Def x) = toFuncDef x
    createAndCall n = [ IABx OpClosure 2 n
                      , IABC OpMove 0 0 0
                      , IABC OpCall 2 1 2
                      ]
    regGlobal n = [ IABx OpClosure 2 (n + length xs)
                  , IABx OpSetGlobal 2 (n + 1)
                  ]

------------------- Functions to load at beginning ------------------------

primitives :: [(String, LuaFunc)]
primitives = [ ("*", LuaFunc {startline=0, endline=0, upvals=0, params=0, 
                              vararg=2, maxstack=7, source="@prim*\0",
                   instructions=[ 
                                  IABC  OpNewTable 0 0 0 -- to hold values
                                , IABC  OpVarArg 1 0 0 -- args in reg 1 and up
                                , IABC  OpSetList 0 0 1 -- save args to table
                                , IABx  OpLoadK 1 0 -- load 1 (loop init)
                                , IABC  OpLen 2 0 0 -- load length (loop max)
                                , IABx  OpLoadK 3 0 -- load 1 (loop step)
                                , IABx  OpLoadK 5 0 -- load unit
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
                              vararg=2, maxstack=7, source="@prim+\0",
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
                              vararg=2, maxstack=9, source="@prim-\0",
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
                                      source="@prim-quotient\0",
                    instructions = [ IABC  OpDiv 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("modulo", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                                      source="@prim-modulo\0",
                    instructions = [ IABC  OpMod 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("expt", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                                      source="@prim-expt\0",
                    instructions = [ IABC  OpPow 0 0 1 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("not", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=1, vararg=0, maxstack = 1,
                                      source="@prim-not\0",
                    instructions = [ IABC  OpNot 0 0 0 
                                   , IABC  OpReturn 0 2 0
                                   ],
                    constants    = [],
                    functions    = []})
             , ("=", LuaFunc { startline=0, endline=0, upvals=0, 
                                      params=2, vararg=0, maxstack = 2,
                                      source="@prim=\0",
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
                                      source="@prim<\0",
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
                                      source="@prim>\0",
                    instructions = [ IABC  OpLT 0 1 0 -- if gt then PC++
                                   , IAsBx OpJmp 0 1 -- jmp requred after gt
                                   , IABC  OpLoadBool 0 1 1 -- load true, PC++
                                   , IABC  OpLoadBool 0 0 0 -- load false  
                                   , IABC  OpReturn 0 2 0 
                                   ],
                    constants    = [],
                    functions    = []})
            ]


------------------------- Main Functions -----------------------------
data Method = M1 | M2 deriving (Eq, Show)

compileFromFile :: Method -> String -> IO (Maybe LuaFunc)
compileFromFile M1 = (fmap . fmap) toFuncProgram . parseFromFile parProgram
compileFromFile M2 = (fmap . fmap) genProgram . parseFromFile file

parseAndWrite :: Method -> String -> String -> IO ()
parseAndWrite m inp out = compileFromFile m inp >>= writeLuaFunc out

writeLuaFunc :: String -> Maybe LuaFunc -> IO ()
writeLuaFunc f ml = case ml >>= finalBuilder of
                       Just bs -> writeBuilder f bs
                       Nothing -> print "error completing builder"

------------------------- Older Compiler ---------------------------------
-- | Converts a Scheme value into a Lua chunk. The chunk takes no parameters, 
-- but expects a boolgle upvalue which is a table of the defined atoms. Entry 0 
-- in the table points to the table for the environment one level up. The chunk
-- returns a single value which is the result of evaluating the value (or
-- possibly no value e.g. in the case of define).
--
genEval :: Value -> LuaFunc
genEval (Number n) = LuaFunc{ startline=1, endline=1, upvals=1, 
                              params=0, vararg=0, maxstack=1, source="@old\0",
                 instructions=[ 
                                 IABx  OpLoadK 0 0 -- Load n
                              , IABC  OpReturn 0 2 0 -- Return n
                              ],   
                 constants=   [ LuaNumber . fromIntegral $ n ], 
                 functions=   []}

genEval (Boolean b) = let val = if b then 1 else 0
                      in  LuaFunc{ startline=1, endline=1, upvals=1,
                                   params=0, vararg=0, maxstack=1, 
                                   source="@old\0",
              instructions = [ IABC  OpLoadBool 0 val 0
                             , IABC  OpReturn 0 2 0
                             ],
              constants    = [],
              functions    = []}

genEval (Atom x) = LuaFunc{ startline=2, endline=2, upvals=1,
                                params=0, vararg=0, maxstack=1, source="@old\0",
          instructions = [ IABx  OpClosure 0 0 -- closure to look up x
                         , IABC  OpGetUpVal 0 0 0 -- pass env table as upval
                         , IABC  OpCall 0 1 2 -- call lookup closure
                         , IABC  OpReturn 0 2 0 -- return lookup of x
                         ],
          constants =    [ ],
          functions =    [ lookupEnv x ]}

genEval (List [Atom "define", Atom x, f]) = 
                    LuaFunc{ startline=3, endline=3, upvals=1,
                             params=0, vararg=0, maxstack=3, source="@old\0",
            instructions = [ IABC  OpNewTable 0 0 0 -- new env table
                           , IABC  OpGetUpVal 1 0 0 -- old env table to reg 1
                           , IABC  OpSetTable 0 257 1 -- point new env to old
                           , IABx  OpClosure 2 0 -- closure to evaluate f
                           , IABC  OpMove 0 0 0 -- pass new table as env
                           , IABC  OpCall 2 1 2 -- call closure
                           , IABC  OpSetTable 1 256 2 -- set x in old table
                           , IABC  OpReturn 0 1 0 -- return nothing
                           ],
            constants =    [ LuaString x
                           , LuaNumber 0
                           ],
            functions =    [genEval f]}

genEval (List [Atom "lambda", List vars, f]) = 
  LuaFunc{ startline=4, endline=4, upvals=1, 
           params=0, vararg=0, maxstack = 1, source="@old\0",
    instructions = [ IABx  OpClosure 0 0 -- closure that evals f with params
                   , IABC  OpGetUpVal 0 0 0 -- pass enboolable
                   , IABC  OpReturn 0 2 0 -- return the closure
                   ],
    constants    = [],
    functions    = [genLambda vars f]}

genEval (List [Atom "if", cond, exp1, exp2]) = 
    LuaFunc{ startline=4, endline=4, upvals=1, params=0, vararg=0, 
              maxstack = 4, source="@old\0",
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
    functions    = [ genEval cond, genEval exp1, genEval exp2 ]}

genEval (List [Atom "register-global", Atom x]) =
  LuaFunc{ startline=0, endline=0, upvals=1, params=0, vararg=0, maxstack=1,
           source="@old\0",
    instructions = [ IABx  OpClosure 0 0
                   , IABC  OpGetUpVal 0 0 0
                   , IABC  OpCall 0 1 2
                   , IABx  OpSetGlobal 0 0
                   , IABC  OpReturn 0 1 0
                   ],
    constants    = [ LuaString x ],
    functions    = [ genEval (Atom x) ]}

genEval (List (f:xs)) = let nvars = length xs
 in 
    LuaFunc { startline=6, endline=6, upvals=1, params=0, vararg=0, 
              maxstack = fromIntegral nvars + 3, source="@old\0",
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
      functions =    genEval f : map genEval xs}

-- | `genLambda` takes a list of `Atom`s (as `Value`s) and a `Value` containing
-- those `Atom`s. It creates a chunk which takes a parameter for each value, 
-- binds each parameter to the corresponding `Atom` and the evaluates the final
-- `Value`.
--
genLambda :: [Value] -> Value -> LuaFunc
genLambda vars f = 
  let nvars = length vars
  in  
                  LuaFunc{ startline=5, endline=5, upvals=1, 
                           params = fromIntegral nvars, vararg=0, 
                           maxstack = fromIntegral nvars + 2, source="@old\0",
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
      constants    = map (\(Atom x) -> LuaString x) vars ++ [LuaNumber 0], 
      functions    = [genEval f]}

-- |Tabool two params, second is a table.
-- Looks up index n in the table and if it is nil return (lookup 0, lookup 0)
-- if the value is not nil return (nil, val)
-- The idea is that 0 points to the next environment up so we could call again
--
lookupOnce :: String -> LuaFunc
lookupOnce s = LuaFunc { startline=0, endline=0, upvals=0, params=2,
                            vararg=0, maxstack=5, source="@old\0",
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
     constants       = [ LuaString s
                       , LuaNumber 0 ],
     functions       = []}

-- |Assumes table is first upval, looks up the index and traces back to previous
-- environments if nil. Returns nil if not present in any table. Should this be
-- changed to lookup in global env if still nil?
--
lookupEnv :: String -> LuaFunc
lookupEnv s =  LuaFunc { startline=0, endline=0, upvals=1, 
                         params=0, source="@old\0",
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
        constants    = [ LuaString s
                       , LuaNumber 0
                       ],
        functions    = [lookupOnce s]}

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
genProgram vs = let atoms = nub $ foldMap getAtoms vs 
                  -- ^list of atoms we'll use 
                    funcs = addPrim atoms : map genEval vs
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
                             constants=cns, functions=funcs, source="@old\0"}

addPrim :: [String] -> LuaFunc
addPrim atoms = 
  --     inxfunc = foldMap (\(x,f) -> maybeToList ((,) f <$> M.lookup x t)) 
  --                 $ primitives -- table inx, function tuples
  --     funcs = map fst inxfunc -- list of funcs
  --     inxs = map (LuaNumber . fromIntegral . snd) inxfunc -- list of indexes
  --     np = length funcs
  let
      usedPrims = filter ((`elem` atoms) . fst) primitives
      (labels, funcs) = unzip usedPrims
      np = length funcs
  in  
    LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, maxstack=2,
              source="@addPrim\0",
      instructions = [ IABC  OpGetUpVal 0 0 0
                     ]
                     ++ (foldMap (\n -> [ IABx  OpClosure 1 n
                                        , IABC  OpSetTable 0 (256+n) 1 ]) 
                                [0..np-1])
                     ++
                     [ IABC  OpReturn 0 1 0 
                     ],
      constants    = map LuaString labels,
      functions    = funcs}

------------------------- Probably not for production ------------------------
result2maybe :: Result a -> Maybe a
result2maybe (Success x) = Just x
result2maybe (Failure x) = Nothing

luafunc :: IO (Maybe LuaFunc)
luafunc = (fmap . fmap) genProgram $ parseFromFile file "samplecode"

string2File :: String -> String -> IO ()
string2File inp f = writeLuaFunc f r
  where r = result2maybe 
          $ fmap toFuncProgram 
          $ parseString parProgram mempty inp

-- | Use    or debugging??? Should probably rewrite without the VM loop since
-- we know the length beforehand.
--
copyTable :: Int -> LuaFunc
copyTable n = LuaFunc { startline=0, endline=0, upvals=1, params=0, vararg=0, 
                        maxstack=7, source="@copyTable\0",
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
                         maxstack=7, source="@printTable\0",
    instructions = [ IABC  OpGetUpVal 0 0 0 --     nv table
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
                               , source = source func
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
                   maxstack=7, source="@sumfunc\0",
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
                   maxstack=6, source="@sampleFunc\0", 
                   instructions=[ IABC OpNewTable 0 0 0
                                , IABC OpSetTable 0 257 259
                                , IABC OpNewTable 1 0 0
                                , IABC OpSetTable 1 258 0
                                , IABC OpSetTable 1 257 260
                                , IABx OpGetGlobal 2 0
                                , IABx OpClosure 3 0
                                , IABC OpMove 0 1 0
                                , IABx OpLoadK 4 1
                                , IABx OpLoadK 5 5
                                , IABC OpCall 3 3 2
                                , IABC OpCall 2 2 1
                                , IABC OpReturn 0 1 0
                                ], 
                   
                   constants=   [ 
                                  LuaString "print"
                                , LuaString "foo"
                                , LuaNumber 0
                                , LuaString "in upper table"
                                , LuaString "in lower table"
                                , LuaNumber 1
                                ],
                   
                   functions=   [ luaLookup
                                ]}

experiment :: IO ()
experiment = case finalBuilder sampleFunc of
                  Just bs -> writeBuilder "temp" bs
                  Nothing -> print "error completing builder"
