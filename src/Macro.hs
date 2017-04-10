module Macro where

import Parser2
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Control.Applicative (liftA2)

-- | Convert an expression to pure datum. TODO: change the name of this 
-- function.
--
convMacro :: GenExpr () -> GenDatum ()
convMacro (Var s ()) = SimpleDatum (Var s ())
convMacro (Literal (LitQuote x)) =
  CompoundDatum $ (SimpleDatum $ Var "quote" ()) : [x]
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

-- | Result of matching a pattern with a datum. If a variable appears without
-- in ellipses in the pattern it will show up in the map with the datum it
-- matched against. If a variable has ellipses it will show up in the lists with
-- each datum it matched with.
--
data Matched a = Matched (M.Map String (GenDatum a)) [Matched a]
  deriving (Eq, Show)

-- | zipWith, but we take values from the longer list after the shorter list has
-- ended.
--
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
match (PatEllipses patterns pattern) (CompoundDatum ds) =
  let n = length patterns
  in  if length ds < n
      then Nothing
      else do
        a <- (match (PatternComp patterns) (CompoundDatum (take n ds)))
        b <- Matched M.empty <$> (traverse (match pattern) (drop n ds))
        return $ combine a b
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

-- | Fill in template Element with matched data. If it is an ellipses element,
-- fill in with each Matched in the list and take all the results that worked.
--
useTemplateElem :: TempElement -> Matched () -> Maybe [GenDatum ()]
useTemplateElem (PureTemp t) m = pure <$> useTemplate t m
useTemplateElem (TempEllipses t) (Matched _ ls) = 
  Just . catMaybes $ map (useTemplate t) ls

-- | fill in template with Matched data.
useTemplate :: Template -> Matched () -> Maybe (GenDatum ())
useTemplate (TemplateComp xs) m = 
  CompoundDatum . concat <$> mapM (flip useTemplateElem m) xs
useTemplate (TemplateDat x) _ = Just $ SimpleDatum (Literal x)
useTemplate (TemplateId s) (Matched m l) = M.lookup s m
useTemplate (TemplateLit s) _ = Just $ SimpleDatum (Var s ())

-- | Match against the pattern in the SyntaxRule and then fill in the template
-- from the match.
--
applyMacro :: SyntaxRule -> GenDatum () -> Maybe (GenDatum ())
applyMacro s dat = match (pat s) dat >>= useTemplate (temp s)

type MacroList = [SyntaxRule]

-- | Try to appyl multiple SyntaxRules and when one fails, recursively apply
-- them to the children in the AST.
--
applyMacros :: MacroList -> GenDatum () -> GenDatum ()
applyMacros ms ex = 
  let conversions = map (flip applyMacro ex) ms
  in  case take 1 $ catMaybes conversions of
        [x] ->  applyMacros ms x
        [] -> case ex of
          CompoundDatum ds -> CompoundDatum $ map (applyMacros ms) ds
          SimpleDatum y -> SimpleDatum y

convDatum :: GenDatum () -> GenExpr ()
convDatum (SimpleDatum x) = x
convDatum (CompoundDatum (SimpleDatum (Var "if" ()) : [a,b,c])) = 
  Cond (convDatum a) (convDatum b) (convDatum c)
convDatum (CompoundDatum (SimpleDatum (Var "lambda" ()) 
  : (CompoundDatum xs) : ys)) = 
    Lambda (map convDatum xs) (convDatumBody ys)
convDatum (CompoundDatum ( SimpleDatum (Var "set!" ()) 
                         : SimpleDatum (Var x ()) 
                         : [y])) = 
  Assign (Var x ()) (convDatum y)
convDatum (CompoundDatum (SimpleDatum (Var "quote" ()) : [x])) =
  Literal $ LitQuote x
convDatum (CompoundDatum (x: xs)) =
  Call (convDatum x) (map convDatum xs)

isDef :: GenDatum a -> Bool
isDef (CompoundDatum (SimpleDatum (Var "define" _) : _)) = True
isDef (CompoundDatum (SimpleDatum (Var "begin" _ ) : xs)) = and $ map isDef xs
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

applyMacrosExpr :: MacroList -> GenExpr () -> GenExpr ()
applyMacrosExpr ms = convDatum . applyMacros ms . convMacro

applyMacrosDef :: MacroList -> GenDef () -> GenDef ()
applyMacrosDef ms = convDatumDef . applyMacros ms . convMacroDef

applyMacrosProgram :: MacroList -> [CommOrDef] -> [CommOrDef]
applyMacrosProgram ms xs = map (\x ->
    case x of
      Comm y -> Comm $ applyMacrosExpr (ms ++ newMacros) y
      Def y -> Def $ applyMacrosDef (ms ++ newMacros) y) $ filter notDef xs
  where
    getSynRule (DefSyn rs) = rs
    getSynRule _ = []
    newMacros = concatMap getSynRule xs
    notDef (DefSyn _) = False
    notDef _ = True


defaultMacros :: MacroList
defaultMacros =
  [ SyntaxRule { --convert `let` to a lambda application
      pat =
        PatEllipses
          [ PatternLit "let"
          , PatEllipses []
              (PatternComp [ PatternId "a"
                           , PatternId "b"
                           ])
          , PatternId "c"]
          (PatternId "d")
    , temp = 
        TemplateComp
          [ PureTemp $ TemplateComp
            [ PureTemp $ TemplateLit "lambda"
            , PureTemp $ TemplateComp $ [TempEllipses $ TemplateId "a"]
            , PureTemp $ TemplateId "c"
            , TempEllipses $ TemplateId "d"
            ]
          , TempEllipses $ TemplateId "b"
          ]
    }

  , SyntaxRule { -- convert `delay` to a lambda
      pat =
        PatternComp [ PatternLit "delay"
                    , PatternId "a"
                    ]
    , temp =
        TemplateComp [ PureTemp $ TemplateLit "lambda"
                     , PureTemp $ TemplateComp []
                     , PureTemp $ TemplateId "a"
                     ]
    }
  ]