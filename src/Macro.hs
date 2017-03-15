module Macro where

import Parser2
import qualified Data.Map as M

call2Macro :: GenExpr a -> GenExpr a
call2Macro (Call (Var x _) xs) = MacroUse (Var x ()) (map convMacro xs)

convMacro :: GenExpr a -> GenDatum a
convMacro (Var s a) = SimpleDatum (Var s a)
convMacro (Literal x) = SimpleDatum (Literal x)
convMacro (Call (Var s a) xs) = CompoundDatum $ SimpleDatum (Var s a)
                                  : map convMacro xs

convAllMacros :: GenExpr a -> GenExpr a
convAllMacros (Var x y) = Var x y
convAllMacros (Literal x) = Literal x
convAllMacros c@(Call (Var x y) xs)
  | x `elem` macros = call2Macro c
  | otherwise = Call (Var x y) (map convAllMacros xs)
convAllMacros (Call f xs) = Call (convAllMacros f) (map convAllMacros xs)
convAllMacros (Lambda xs b) = Lambda xs (convMacroBody b)
convAllMacros (Cond a b c) = Cond (convAllMacros a) (convAllMacros b)
                                      (convAllMacros c)

convMacroBody :: GenBody a -> GenBody a
convMacroBody (Body ds es) = Body (map convMacroDef ds) (map convAllMacros es)

convMacroDef :: GenDef a -> GenDef a
convMacroDef (Def1 x y) = Def1 x (convAllMacros y)
convMacroDef (Def2 x ys b) = Def2 x ys (convMacroBody b)
convMacroDef (Def3 ds) = Def3 $ map convMacroDef ds

macros = ["or"]

data Pattern = PatternId String | PatternDat Lit | PatternComp [Pattern]
  deriving (Eq, Show)

match :: Pattern -> GenDatum a -> Maybe (M.Map String (GenDatum a))
match (PatternComp ps) (CompoundDatum ds) = 
  if length ps == length ds
    then fmap M.unions . sequence $ zipWith match ps ds
    else Nothing
match (PatternDat x) (SimpleDatum (Literal y)) = 
  if x == y
    then Just M.empty
    else Nothing
match (PatternId s) x = Just $ M.singleton s x
match _ _ = Nothing

useTemplate :: Pattern -> M.Map String (GenDatum ()) -> GenDatum ()
useTemplate (PatternComp xs) m = CompoundDatum $ map (flip useTemplate m) xs
useTemplate (PatternDat x) _ = SimpleDatum (Literal x)
useTemplate (PatternId s) m = case M.lookup s m of
  Nothing -> SimpleDatum (Var s ())
  Just d -> d

convDatum :: GenDatum () -> GenExpr ()
convDatum (SimpleDatum x) = x
convDatum (CompoundDatum (SimpleDatum (Var "if" _) : [a,b,c])) = 
  Cond (convDatum a) (convDatum b) (convDatum c)
convDatum (CompoundDatum (SimpleDatum (Var "lambda" _) : xs : ys)) = undefined
convDatum (CompoundDatum ( SimpleDatum (Var "set!" _) 
                         : SimpleDatum (Var x _) 
                         : [y])) = 
  Assign (Var x ()) (convDatum y)
convDatum (CompoundDatum (SimpleDatum (Var x _) : xs)) =
  Call (Var x ()) (map convDatum xs)

isDef :: GenDatum a -> Bool
isDef (CompoundDatum (SimpleDatum (Var "define" _) : _)) = True
isDef (CompoundDatum (SimpleDatum (Var "begin" _) : xs)) = and $ map isDef xs
isDef _ = False

convDatumDef :: GenDatum () -> GenDef ()
convDatumDef (CompoundDatum (SimpleDatum (Var "begin" ()) : xs)) = 
  Def3 $ map convDatumDef xs
convDatumDef (CompoundDatum (SimpleDatum (Var "define" ()) : 
               CompoundDatum (SimpleDatum (Var x ()) : vs) :
               [b])) = 
  Def2 (Var x ()) (map convDatum vs) (convDatumBody b)
convDatumDef (CompoundDatum (SimpleDatum (Var "define" ()) : x : [y])) =
  Def1 (convDatum x) (convDatum y)

convDatumBody :: GenDatum () -> GenBody ()
convDatumBody (SimpleDatum x) = Body [] [x]
convDatumBody (CompoundDatum (x:xs)) = 
  if isDef x 
    then let  Body ds es = convDatumBody (CompoundDatum xs)
         in   Body (convDatumDef x : ds) es
    else Body [] (map convDatum (x:xs))  

replaceMacros :: GenExpr a -> GenExpr a
replaceMacros = undefined