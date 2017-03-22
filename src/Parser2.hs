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
               , "case", "let*", "letrec", "do", "delay", "quasiquote" 
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
          deriving (Eq, Show)

-- | Data type for a Scheme expression. The a contains additional annotations.
-- 
data GenExpr a = Var String a
          | Literal Lit
          | Call (GenExpr a) [GenExpr a]
          | Lambda [GenExpr ()] (GenBody a) 
            -- ^ first list is variabls, should only be Vars   
          | Cond (GenExpr a) (GenExpr a) (GenExpr a)
          | Assign (GenExpr ()) (GenExpr a)
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

-- | Map of variable names to scoping. Int = 0 indicates immediate environment and
-- Int > 0 indicates the number of scopes outward.
--
type VarT = M.Map String Int

type Expr = GenExpr ()
type Def = GenDef ()
type Body = GenBody ()
type CommOrDef = GenCommOrDef ()
type AnnExpr = GenExpr VarT
type AnnDef = GenDef VarT 
type AnnBody = GenBody VarT
type AnnCommOrDef = GenCommOrDef VarT

parExpr :: Parser Expr
parExpr = parVar 
        <|> try parLit 
        <|> try parCall 
        <|> try parLambda 
        <|> try parCond
        <|> parAssign 

-- | Parse a literal without wrapping, so it can be used for patterns,
-- templates, or expressions.
--
parLitRaw :: Parser Lit
parLitRaw = try (do
              x <- parToken
              if lit x then return (conv x) else empty)
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

-- | Which variables appear free in the definition?
--
freeInDef :: GenDef a -> S.Set String
freeInDef (Def1 _ e) = freeVarsEx e
freeInDef (Def2 _ es b) = freeVars b `S.difference` foldMap freeVarsEx es
freeInDef (Def3 es) = foldMap freeInDef es

-- | Free variables in expression
--
freeVarsEx :: GenExpr a -> S.Set String
freeVarsEx (Var s _) = S.singleton s
freeVarsEx (Literal _) = S.empty
freeVarsEx (Call a b) = S.union (freeVarsEx a) (foldMap freeVarsEx b)
freeVarsEx (Lambda vars body) = S.difference (freeVars body) 
                                  (foldMap freeVarsEx vars)
freeVarsEx (Cond x y z) = foldMap freeVarsEx [x, y, z]
freeVarsEx (Assign a b) = S.union (freeVarsEx a) (freeVarsEx b)

-- | Free variables in body
--
freeVars :: GenBody a -> S.Set String
freeVars (Body defs exprs) = S.union (foldMap freeVarsEx exprs)
                                   (foldMap freeInDef defs)
                             `S.difference` foldMap definedVars defs

-- | The variables defined at the top level are all put in a map with value
-- 0 to indicate they are defined in the current environment.
--
topDefined :: [GenCommOrDef a] -> VarT
topDefined xs = foldr helper (M.fromList []) (map getVars xs)
  where
    helper ::S.Set String -> M.Map String Int -> M.Map String Int
    helper s m = M.union m (M.fromList $ zip (S.toList s) [0,0..])
    getVars :: GenCommOrDef a -> S.Set String
    getVars (Comm _) = S.empty
    getVars (Def d) = definedVars d

-- | Given an annotation of variable environments, extend it to an expression
-- 
annotateEx :: VarT -> GenExpr a -> AnnExpr
annotateEx t (Var s _) = Var s t
annotateEx _ (Literal x) = Literal x
annotateEx t (Call f xs) = Call (annotateEx t f) (map (annotateEx t) xs)
annotateEx t (Lambda es b) = Lambda es (annotateBody newt b)
 -- ^ increase the scope by 1 for each var not in es.
  where
    newt = M.unionWith min 
            ((+1) <$> t) 
            (M.fromList $ zip (S.toList $ foldMap freeVarsEx es) [0,0..])  
annotateEx t (Cond a b c) = Cond (annotateEx t a) (annotateEx t b)
                                                   (annotateEx t c)
annotateEx t (Assign a b) = Assign a (annotateEx t b)

annotateBody :: VarT -> GenBody a -> AnnBody
annotateBody t (Body ds es) = Body (map (annotateDef newt) ds) 
                                   (map (annotateEx newt) es)
  where
    newt = M.unionWith min t
            (M.fromList $ zip (S.toList $ foldMap definedVars ds) [0,0..])

annotateDef :: VarT -> GenDef a -> AnnDef
annotateDef t (Def1 a b) = Def1 a (annotateEx t b)
annotateDef t (Def2 a b c) = Def2 a b (annotateBody newt c)
  where
    newt = M.unionWith min 
             ((+1) <$> t)
             (M.fromList $ zip (S.toList $ foldMap freeVarsEx b) [0,0..])
annotateDef t (Def3 ds) = Def3 $ map (annotateDef newt) ds
  where
    newt = M.unionWith min t
            (M.fromList $ zip (S.toList $ foldMap definedVars ds) [0,0..])

annotateProgram :: VarT -> [GenCommOrDef a] -> [AnnCommOrDef]
annotateProgram t xs = map (ann newt) xs
  where
    ann t (Def d) = Def $ annotateDef t d
    ann t (Comm e) = Comm $ annotateEx t e
    newt = M.unionWith min t (topDefined xs)

allVars :: [GenCommOrDef a] -> S.Set String
allVars = foldMap getFree
  where
    getFree (Comm x) = freeVarsEx x
    getFree (Def x) = freeInDef x