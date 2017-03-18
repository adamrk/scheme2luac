module Macro where

import Parser2
import qualified Data.Map as M
import Data.Maybe (isJust)
import Control.Applicative (liftA2)

convMacro :: GenExpr () -> GenDatum ()
convMacro (Var s a) = SimpleDatum (Var s a)
convMacro (Literal x) = SimpleDatum (Literal x)
convMacro (Call x xs) = CompoundDatum $ convMacro x : map convMacro xs
convMacro (Lambda vs b) = 
  let bs = convMacroBody b
  in  CompoundDatum $ SimpleDatum (Var "lambda" ())
      : CompoundDatum (map convMacro vs) : bs
convMacro (Cond a b c) = CompoundDatum $ SimpleDatum (Var "if" ())
  : convMacro a : convMacro b : [convMacro c]
convMacro (Assign a b) = CompoundDatum $ SimpleDatum (Var "set!" ())
  : convMacro a : [convMacro b]

convMacroBody :: GenBody () -> [GenDatum ()]
convMacroBody (Body ds es) = map convMacroDef ds ++ map convMacro es

convMacroDef :: GenDef () -> GenDatum ()
convMacroDef (Def1 x y) = CompoundDatum $ [ SimpleDatum (Var "define" ())
                                          , SimpleDatum x
                                          , convMacro y
                                          ]
convMacroDef (Def2 x ys b) = CompoundDatum $ [ SimpleDatum (Var "define" ())
                                             , CompoundDatum $ SimpleDatum x :
                                                map SimpleDatum ys
                                             ] ++ convMacroBody b
convMacroDef (Def3 ds) = CompoundDatum $ SimpleDatum (Var "begin" ()) : 
  map convMacroDef ds                                      

data Pattern = PatternId String 
             | PatternDat Lit 
             | PatternComp [Pattern]
             | PatternLit String
             | Ellipses
  deriving (Eq, Show)

data Matched a = Matched (M.Map String (GenDatum a)) [Matched a]
  deriving (Eq, Show)

extZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
extZipWith f (b:bs) (c:cs) = f b c : extZipWith f bs cs
extZipWith f [] cs = cs
extZipWith f bs [] = bs

-- | Combine two match results
--
combine :: Matched a -> Matched a -> Matched a
combine (Matched m1 l1) (Matched m2 l2) = 
  Matched (M.union m1 m2) (extZipWith combine l1 l2)

-- | Fills in a pattern from a GenDatum that matches it
--
match :: Pattern -> GenDatum a -> Maybe (Matched a)
match (PatternComp [x, Ellipses]) (CompoundDatum ds) = 
  Matched M.empty <$> traverse (match x) ds
match (PatternComp (x:xs)) (CompoundDatum (d:ds)) = 
  combine <$> (match x d) <*> (match (PatternComp xs) (CompoundDatum ds))
match (PatternComp []) (CompoundDatum []) = Just $ Matched M.empty []
match (PatternComp []) _ = Nothing
match (PatternDat x) (SimpleDatum (Literal y)) = 
  if x == y
    then Just $ Matched M.empty []
    else Nothing
match (PatternId s) x = Just $ Matched (M.singleton s x) []
match (PatternLit s) (SimpleDatum (Var x _)) = if x == s 
                                              then Just $ Matched M.empty [] 
                                              else Nothing 
match _ _ = Nothing

getDat :: GenDatum () -> [GenDatum ()]
getDat (CompoundDatum xs) = xs
getDat _ = []

useTemplate :: Pattern -> Matched () -> Maybe (GenDatum ())
useTemplate (PatternComp [a, Ellipses]) (Matched m ls) =
  CompoundDatum <$> (sequence . filter isJust) (map (useTemplate a) ls)
useTemplate (PatternComp (x:xs)) m = 
  let a = useTemplate x m
      b = useTemplate (PatternComp xs) m
  in  CompoundDatum <$> liftA2 (:) a (getDat <$> b)
useTemplate (PatternComp []) m = Just $ CompoundDatum []
useTemplate (PatternDat x) _ = Just $ SimpleDatum (Literal x)
useTemplate (PatternId s) (Matched m l) = M.lookup s m
useTemplate (PatternLit s) _ = Just $ SimpleDatum (Var s ())

applyMacro :: String -> Pattern -> Pattern -> GenDatum () -> Maybe (GenDatum ())
applyMacro key pat temp dat = match pat dat >>= useTemplate temp

type MacroList = [(String, Pattern, Pattern)]

applyMacros :: MacroList -> GenDatum () -> GenDatum ()
applyMacros ms ex = 
  let conversions = map (\(k, p, t) -> applyMacro k p t ex) ms
  in  case take 1 $ filter isJust conversions of
        [Just x] ->  applyMacros ms x
        [] -> case ex of
          CompoundDatum ds -> CompoundDatum $ map (applyMacros ms) ds
          SimpleDatum y -> SimpleDatum y

convDatum :: GenDatum () -> GenExpr ()
convDatum (SimpleDatum x) = x
convDatum (CompoundDatum (SimpleDatum (Var "if" _) : [a,b,c])) = 
  Cond (convDatum a) (convDatum b) (convDatum c)
convDatum (CompoundDatum (SimpleDatum (Var "lambda" _) 
  : (CompoundDatum xs) : ys)) = 
    Lambda (map convDatum xs) (convDatumBody ys)
convDatum (CompoundDatum ( SimpleDatum (Var "set!" _) 
                         : SimpleDatum (Var x _) 
                         : [y])) = 
  Assign (Var x ()) (convDatum y)
convDatum (CompoundDatum (x: xs)) =
  Call (convDatum x) (map convDatum xs)


isDef :: GenDatum a -> Bool
isDef (CompoundDatum (SimpleDatum (Var "define" _) : _)) = True
isDef (CompoundDatum (SimpleDatum (Var "begin" _) : xs)) = and $ map isDef xs
isDef _ = False

convDatumDef :: GenDatum () -> GenDef ()
convDatumDef (CompoundDatum (SimpleDatum (Var "begin" ()) : xs)) = 
  Def3 $ map convDatumDef xs
convDatumDef (CompoundDatum (SimpleDatum (Var "define" ()) : 
               CompoundDatum (SimpleDatum (Var x ()) : vs) :
               b)) = 
  Def2 (Var x ()) (map convDatum vs) (convDatumBody b)
convDatumDef (CompoundDatum (SimpleDatum (Var "define" ()) : x : [y])) =
  Def1 (convDatum x) (convDatum y)

convDatumBody :: [GenDatum ()] -> GenBody ()
convDatumBody [] = Body [] []
convDatumBody (x:xs) =
  let Body ds es = convDatumBody xs
  in  if isDef x
    then Body (convDatumDef x:ds) es
    else Body ds (convDatum x:es)
-- convDatumBody (CompoundDatum (x:xs)) = 
--   if isDef x 
--     then let  Body ds es = convDatumBody (CompoundDatum xs)
--          in   Body (convDatumDef x : ds) es
--     else Body [] (map convDatum (x:xs))  

applyMacrosExpr :: MacroList -> GenExpr () -> GenExpr ()
applyMacrosExpr ms = convDatum . applyMacros ms . convMacro

applyMacrosDef :: MacroList -> GenDef () -> GenDef ()
applyMacrosDef ms = convDatumDef . applyMacros ms . convMacroDef

applyMacrosProgram :: MacroList -> [CommOrDef] -> [CommOrDef]
applyMacrosProgram ms xs = map (\x ->
  case x of
    Comm y -> Comm $ applyMacrosExpr ms y
    Def y -> Def $ applyMacrosDef ms y) xs

defaultMacros :: MacroList
defaultMacros = 
  [ ( "let"
    , PatternComp [ PatternLit "let" 
                  , PatternComp [ PatternComp [ PatternId "a"
                                              , PatternId "b"
                                              ]
                                , Ellipses
                                ]
                  , PatternId "c"
                  ]
    , PatternComp [ PatternComp [ PatternLit "lambda"
                               , PatternComp [ PatternId "a"
                                             , Ellipses
                                             ]
                               , PatternId "c"
                               ]
                 , PatternId "b"
                 , Ellipses
                 ]
    )
  ]