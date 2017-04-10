{-# LANGUAGE ExistentialQuantification #-}

module Parser2 where

import Text.Trifecta
import Data.Char (isSpace, isAlpha, isAscii, isPrint)
import Data.List (intercalate)
import Control.Applicative ((<|>), empty, liftA2, liftA3)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Map as M

---------------------- Parse Tokens -----------------------------

data Token = Identifier String
           | Boolean' Bool
           | Number' Double
           | Character Char
           | String String
           | LeftPar
           | RightPar
           | Other String
            deriving (Eq, Show)

exprKeywords = [ "quote", "lambda", "if", "set!", "begin", "cond", "and"
               , "case", "let*", "letrec", "do", "quasiquote"
               ]
synKeywords = [ "else", "=>", "define", "unquote", "unquote-splicing" ]
              ++ exprKeywords

parInit :: Parser Char
parInit = oneOf ['a'..'z'] <|> oneOf [ '!', '$', '%', '&', '*', '/', ':', '<'
                                     , '=', '>', '?', '^', '_', '~' ]

parSubseq :: Parser Char
parSubseq = parInit <|> digit <|> oneOf ['+', '-', '.', '@']

parIdent :: Parser String
parIdent = (:) <$> parInit <*> many parSubseq <|> string "+" <|> string "-"
               <|> string "..."

parBool :: Parser String
parBool = string "#t" <|> string "#f"

parNum :: Parser String
parNum = some digit

parChar :: Parser String
parChar = (++) <$> string "#\\" <*> ( string "space"
                                    <|> string "newline"
                                    <|> fmap (:[]) characterChar )

parStr :: Parser String
parStr = char '\"' *> 
           some ( string "\\\"" *> return '\"'
                  <|> string "\\\\" *> return '\\'
                  <|> satisfy (\c -> (c /= '\"' && c /='\\' && c > '\026'))
                )
           <* char '\"' 

parToken :: Parser Token
parToken =   fmap Identifier parIdent 
         <|> fmap boolConv parBool 
         <|> fmap numConv parNum 
         <|> fmap charConv parChar 
         <|> fmap String parStr 
         <|> char '(' *> pure LeftPar
         <|> char ')' *> pure RightPar
         <|> fmap (\x -> Other [x]) (oneOf ['\'', '`', ',', '.'])
         <|> fmap Other (string "#(")
         <|> fmap Other (string ",@")
          where
            boolConv "#f" = Boolean' False
            boolConv "#t" = Boolean' True
            numConv = Number' . read
            charConv "space" = Character ' '
            charConv "newline" = Character '\n'
            charConv (['#', '\\', x]) = Character x

------------------------- Parse Macros ------------------------------

-- | This is the type that is used for pattern matching in a macro.
--
data Pattern = PatternId String -- a variable identifier (matches anything)
             | PatternDat Lit 
             | PatternComp [Pattern]
             | PatternLit String -- an identifier that only matches itself
             | PatEllipses [Pattern] Pattern
  deriving (Eq, Show)

-- | Macro template element to be filled in after the corresponding pattern 
-- was matched against. It is either an template or a template with ellipses
-- which is to be filled in 0 or more times. 
--
data TempElement = PureTemp Template 
                 | TempEllipses Template 
  deriving (Eq, Show) 

-- | Template to fill in after a match.
--
data Template = TemplateId String -- variable to fill with matched data
              | TemplateDat Lit
              | TemplateComp [TempElement]
              | TemplateLit String -- literal identifier filled with itself
  deriving (Eq, Show)

-- | Get a list of the variables used in a pattern.
--
getPatVars :: Pattern -> [String]
getPatVars (PatternId s) = [s]
getPatVars (PatternDat _) = []
getPatVars (PatternComp xs) = concatMap getPatVars xs
getPatVars (PatternLit _) = []
getPatVars (PatEllipses xs x) = concatMap getPatVars xs ++ getPatVars x

parPattern :: [String] -> Parser Pattern
parPattern lits = 
  try (do
    token parLeft
    pats <- many $ token $ parPattern lits
    token parRight
    let initial = init pats
    if PatternId "..." `elem` initial
    then unexpected "ellipses not at pattern end"
    else if last pats == PatternId "..."
      then if length initial >= 1 
        then return $ PatEllipses (init $ initial) (last $ initial)
        else unexpected "no pattern preceding ellipses"
      else return $ PatternComp pats) 
  <|> try (fmap PatternDat parLitRaw)
  <|> try (do
    s <- token parIdent
    if s `elem` lits -- parse identifier as variable Id or literal?
    then return $ PatternLit s
    else return $ PatternId s)
  <|> unexpected "couldn't parse pattern"

parTempElement :: [String] -> Parser TempElement
parTempElement vars = do
  t <- token $ parTemplate vars
  try (token (string "...") >> return (TempEllipses t)) 
    <|> return (PureTemp t) 


parTemplate :: [String] ->  Parser Template
parTemplate vars = 
  try (do
    token parLeft
    ts <- many (token $ parTempElement vars)
    parRight
    return $ TemplateComp ts)
  <|> try (fmap TemplateDat parLitRaw)
  <|> try (do
    s <- token parIdent
    if s `elem` vars -- parse identifier as a var id or a literal id?
    then return $ TemplateId s
    else return $ TemplateLit s)
  <|> unexpected "couldn't parse template"

data SyntaxRule = SyntaxRule { pat :: Pattern, temp :: Template}
  deriving (Eq, Show)

parSynTaxRule :: [String] -> Parser SyntaxRule
parSynTaxRule lits = do
  token parLeft
  p <- token $ parPattern lits
  let vars = getPatVars p
  t <- token $ parTemplate vars
  parRight
  return $ SyntaxRule p t

parDefSyn :: Parser [SyntaxRule]
parDefSyn = do
  token parLeft 
  token $ string "define-syntax"
  key <- token parIdent
  token parLeft
  token $ string "syntax-rules"
  token parLeft
  ids <- many $ token parIdent
  token parRight
  rules <- many $ token (parSynTaxRule (key:ids))
  token parRight
  parRight
  return $ rules

------------------------- Parse Expressions and Defs -----------------

data Lit = LitBool Bool
         | LitNum Double
         | LitChar Char
         | LitStr String
         | LitQuote (GenDatum ())
          deriving (Eq, Show)

-- | Data type for a Scheme expression. The a contains additional annotations.
-- 
data GenExpr a  = Var String a
                | Literal Lit
                | Call (GenExpr a) [GenExpr a]
                | Lambda [GenExpr ()] (GenBody a)
                  -- ^ first list is variabls, should only be Vars 
                | Cond (GenExpr a) (GenExpr a) (GenExpr a)
                | Assign (GenExpr a) (GenExpr a)
                | DerivedExpr
                | MacroUse (GenExpr ()) [GenDatum a]
                | MacroBlock
                 deriving (Eq, Show)

-- | Scheme definition, with annotations in the a type.
--
data GenDef a = Def1 (GenExpr ()) (GenExpr a)
                -- ^ first expr should only be a Var (the variable we define)
              | Def2 (GenExpr ()) [GenExpr ()] (GenBody a)
              | Def3 [GenDef a]
                deriving (Eq, Show)

-- | Schem lambda body, with annotations in the a type.
--
data GenBody a = Body [GenDef a] [GenExpr a] deriving (Eq, Show)

-- | Pure datum type. Anything that parses as a GenExpr should also parse as
-- a GenDatum. This ignores all keywords and simple generates an AST with simple
-- tokens or lists of tokens.
--
data GenDatum a = SimpleDatum (GenExpr a) | CompoundDatum [GenDatum a]
  deriving (Eq, Show)

-- | Top Level piece of a scheme program. Either an expression, definition or 
-- define-syntax.
--
data GenCommOrDef a = Comm (GenExpr a) 
                    | Def (GenDef a) 
                    | DefSyn [SyntaxRule]
  deriving (Eq, Show)

data DefinedEnv = Global | Local
  deriving (Eq, Show)

type Expr = GenExpr ()
type Def = GenDef ()
type Body = GenBody ()
type CommOrDef = GenCommOrDef ()

type AnnExpr = GenExpr DefinedEnv
type AnnDef = GenDef DefinedEnv
type AnnBody = GenBody DefinedEnv
type AnnCommOrDef = GenCommOrDef DefinedEnv

parExpr :: Parser Expr
parExpr = parVar 
        <|> try parLit 
        <|> try parCall 
        <|> try parLambda 
        <|> try parCond
        <|> parAssign 

parDatum :: Parser (GenDatum ())
parDatum = try parSimpleDatum <|> (do
  token parLeft
  xs <- many $ token parDatum
  parRight
  return $ CompoundDatum xs)

parSimpleDatum :: Parser (GenDatum ())
parSimpleDatum = do
  x <- parToken
  case x of
    LeftPar -> unexpected "( is not simple Datum)"
    RightPar -> unexpected ") is not simple datum"
    Other _ -> unexpected "Other is not simple datum"
    Boolean' b -> return $ SimpleDatum (Literal $ LitBool b)
    Number' n -> return $ SimpleDatum (Literal $ LitNum n)
    Character c -> return $ SimpleDatum (Literal $ LitChar c)
    String s -> return $ SimpleDatum (Literal $ LitStr s)
    Identifier s -> return $ SimpleDatum (Var s ())

-- | Parse a literal without wrapping, so it can be used for patterns,
-- templates, or expressions.
--
parLitRaw :: Parser Lit
parLitRaw = try (do
              x <- parToken
              if lit x then return (conv x) else empty)
            <|> try (do
              token parLeft
              x <- token parIdent
              if x == "quote"
                then (do
                  x <- token parDatum <* parRight
                  case x of
                    SimpleDatum (Literal (LitBool b)) -> return $ LitBool b
                    SimpleDatum (Literal (LitNum n)) -> return $ LitNum n
                    SimpleDatum (Literal (LitChar c)) -> return $ LitChar c
                    SimpleDatum (Literal (LitStr s)) -> return $ LitStr s
                    x -> return $ LitQuote x)
                else unexpected "Not a quote")
            <|> try (do
              token $ char '\''
              x <- token parDatum
              case x of
                SimpleDatum (Literal (LitBool b)) -> return $ LitBool b
                SimpleDatum (Literal (LitNum n)) -> return $ LitNum n
                SimpleDatum (Literal (LitChar c)) -> return $ LitChar c
                SimpleDatum (Literal (LitStr s)) -> return $ LitStr s
                x -> return $ LitQuote x
              )
            <|> unexpected "Not a literal"
         where
            lit (Boolean' _) = True
            lit (Number' _) = True
            lit (Character _) = True
            lit (String _) = True
            lit _ = False
            conv (Boolean' x) = LitBool x
            conv (Number' x) = LitNum x
            conv (Character x) = LitChar x
            conv (String x) = LitStr x

parLit :: Parser Expr
parLit = fmap Literal parLitRaw

parLeft :: Parser ()
parLeft = (try $ do
  x <- parToken
  if x == LeftPar then return () else empty
  ) <|> unexpected "Expected ( token"

parRight :: Parser ()
parRight = (try $ do
  x <- parToken
  if x == RightPar then return () else empty
  ) <|> unexpected "Expected ) token"

parCall :: Parser Expr
parCall = do
            token parLeft
            operator <- token parExpr
            operands <- many (token parExpr)
            parRight
            return $ Call operator operands
          <|> unexpected "expected procedure call"

parLambda :: Parser Expr
parLambda = do
              token parLeft
              x <- token parToken
              if x /= Identifier "lambda" then empty else do
                formals <- token parFormals
                body <- token parBody
                parRight
                return $ Lambda formals body

parVar :: Parser Expr
parVar = try $ do
            x <- parToken
            case x of
              Identifier s -> if not (s `elem` synKeywords) 
                                then return (Var s ())
                                else empty
              _ -> empty
         <|> unexpected "expected variable"               


parFormals :: Parser [Expr]
parFormals = try $ (do
              x <- parVar 
              return [x]) 
            <|> (do
              token parLeft
              vars <- many (token parVar)
              parRight
              return vars)

parBody :: Parser Body
parBody = do
  defs <- many (token parDef)
  exprs <- many (token parExpr)
  return $ Body defs exprs 

parSeq :: Parser [Expr]
parSeq = some parExpr

parDef :: Parser Def
parDef = (<|> unexpected "not a definition" ) $ try $ do
  token parLeft
  x <- token parIdent
  if x == "define" then (try (do
      v <- token parVar
      e <- token parExpr
      parRight
      return $ Def1 v e)
    <|> (do
      token parLeft
      v <- token parVar
      formals <- many (token parVar)
      token parRight
      body <- parBody
      parRight
      return (Def2 v formals body)))
  else (if x /= "begin" then empty else
    do
      defs <- many (token parDef)
      parRight
      return (Def3 defs) )

parCond :: Parser Expr
parCond = (<|> unexpected "no if statement") $ try $ do
  token parLeft
  x <- token parIdent
  if x /= "if" then empty else do
    test <- token parExpr
    conseq <- token parExpr
    altern <- token parExpr
    parRight
    return $ Cond test conseq altern

parAssign :: Parser Expr
parAssign = (<|> unexpected "not an assignment") $ try $ do
  token parLeft
  x <- token parIdent
  if x /= "set!" then empty else do
    var <- token parVar
    expr <- token parExpr
    parRight
    return $ Assign var expr

parProgram :: Parser [CommOrDef]
parProgram = many $ try (fmap Comm $ token parExpr) 
                    <|> try (fmap Def $ token parDef)
                    <|> try (fmap DefSyn $ token parDefSyn)

---------------- Annotating functions ------------------------

-- | Which variables are being defined in this definition?
--
definedVars :: GenDef a -> S.Set String
definedVars (Def1 (Var s _) _) = S.singleton s
definedVars (Def2 (Var s _) _ _) = S.singleton s
definedVars (Def3 ds) = foldMap definedVars ds

-- | Which variables appear free in the definition? But only those for which `f`
-- evaluates to True. So we can use the annotation to filter which types of
-- variables we want.
--
freeInAnnDef :: (a -> Bool) -> GenDef a -> S.Set String
freeInAnnDef f (Def1 _ e) = freeVarsAnnEx f e
freeInAnnDef f (Def2 _ es b) =
  freeVarsAnnBody f b `S.difference` foldMap freeVarsEx es
freeInAnnDef f (Def3 es) = foldMap (freeInAnnDef f) es

freeInDef :: GenDef a -> S.Set String
freeInDef = freeInAnnDef (const True)

-- | Free variables in expression, but only those for which `f` evaluates to
-- True. So we can use the annotation to filter which types of variables
-- we want.
--
freeVarsAnnEx :: (a -> Bool) -> GenExpr a -> S.Set String
freeVarsAnnEx f (Var s a) = if f a then S.singleton s else S.empty
freeVarsAnnEx _ (Literal _) = S.empty
freeVarsAnnEx f (Call a b) =
  S.union (freeVarsAnnEx f a) (foldMap (freeVarsAnnEx f) b)
freeVarsAnnEx f (Lambda vars body) = S.difference (freeVarsAnnBody f body)
                                  (foldMap freeVarsEx vars)
freeVarsAnnEx f (Cond x y z) = foldMap (freeVarsAnnEx f) [x, y, z]
freeVarsAnnEx f (Assign a b) = S.union (freeVarsAnnEx f a) (freeVarsAnnEx f b)

-- | Free variables in expression
--
freeVarsEx :: GenExpr a -> S.Set String
freeVarsEx = freeVarsAnnEx (const True)

-- | Free variables in body, But only those for which `f`
-- evaluates to True. So we can use the annotation to filter which types of
-- variables we want.
--
freeVarsAnnBody :: (a -> Bool) -> GenBody a -> S.Set String
freeVarsAnnBody f (Body defs exprs) = S.union (foldMap (freeVarsAnnEx f) exprs)
                                             (foldMap (freeInAnnDef f) defs)
                                       `S.difference` foldMap definedVars defs

-- | All free variables in a body.
--
freeVars :: GenBody a -> S.Set String
freeVars = freeVarsAnnBody (const True)

-- | Free vars in the expression that have been annotated as nonglobal
--
freeNonGlobalVars :: AnnExpr -> S.Set String
freeNonGlobalVars = freeVarsAnnEx (== Local)

-- | Annotate each expression by labeling each lambda with the variables in 
-- needs from the environment.
-- 
annotateEx :: S.Set String -> GenExpr a -> AnnExpr
annotateEx vars (Var s _)
    | s `S.member` vars = Var s Local
    | otherwise = Var s Global
annotateEx _ (Literal x) = Literal x
annotateEx vars (Call f xs) = 
  Call (annotateEx vars f) (map (annotateEx vars) xs)
annotateEx vars (Lambda es b) = Lambda es (annotateBody newVars b)
  where
    newVars = S.unions (vars : map freeVarsEx es) 
     -- ^ add lambda variables to defined vars set
annotateEx vars (Cond a b c) = 
  Cond (annotateEx vars a) (annotateEx vars b) (annotateEx vars c)
annotateEx vars (Assign a b) = Assign (annotateEx vars a) (annotateEx vars b)

annotateBody :: S.Set String -> GenBody a -> AnnBody
annotateBody vars (Body ds es) = 
  Body (map (annotateDef vars) ds) (map (annotateEx newVars) es)
  where
    newVars = S.unions (vars : map definedVars ds)

annotateDef :: S.Set String -> GenDef a -> AnnDef
annotateDef vars (Def1 a b) = 
  Def1 a $ annotateEx (S.union vars $ freeVarsEx a) b
annotateDef vars (Def2 a b c) = Def2 a b (annotateBody newVars c)
  where
    newVars = S.unions (vars : freeVarsEx a : map freeVarsEx b)
annotateDef vars d@(Def3 ds) = 
  Def3 $ map (annotateDef (S.union vars $ definedVars d)) ds

allDefined :: [GenCommOrDef a] -> S.Set String
allDefined = foldMap definedVars'
  where
    definedVars' (Comm _) = S.empty
    definedVars' (Def x) = definedVars x

annotateProgram :: [GenCommOrDef a] -> [AnnCommOrDef]
annotateProgram xs = map ann xs
  where
    ann (Comm x) = Comm $ annotateEx (allDefined xs) x
    ann (Def x) = Def $ annotateDef (allDefined xs) x

-- | All variables free in the program
--
allVars :: [GenCommOrDef a] -> S.Set String
allVars = foldMap getFree
  where
    getFree (Comm x) = freeVarsEx x
    getFree (Def x) = freeInDef x