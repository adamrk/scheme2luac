module Parser2 where

import Text.Trifecta
import Data.Char (isSpace, isAlpha, isAscii, isPrint)
import Data.List (intercalate)
import Control.Applicative ((<|>), empty, liftA2, liftA3)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Map as M

data Token = Identifier String 
           | Boolean' Bool
           | Number' Double 
           | Character Char
           | String String
           | LeftPar 
           | RightPar
           | Other String
            deriving (Eq, Show)

exprKeywords = [ "quote", "lambda", "if", "set!", "begin", "cond", "and", "or"
               , "case", "let", "let*", "letrec", "do", "delay", "quasiquote" 
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

data Expr = Var String
          | Literal Lit
          | Call Expr [Expr]
          | Lambda [Expr] Body 
          | Cond Expr Expr Expr
          | Assign Expr Expr
          | DerivedExpr
          | MacroUse
          | MacroBlock
           deriving (Eq, Show)

data Lit = LitBool Bool
         | LitNum Double
         | LitChar Char
         | LitStr String
          deriving (Eq, Show)

data Def = Def1 Expr Expr
         | Def2 Expr [Expr] Body
         | Def3 [Def]
          deriving (Eq, Show)

data Body = Body [Def] [Expr]
            deriving (Eq, Show)

data CommOrDef = Comm Expr | Def Def deriving (Eq, Show)

type VarT = M.Map String Int

data AnnExpr = AVar String VarT
             | ALiteral Lit
             | ACall AnnExpr [AnnExpr]
             | ALambda [Expr] AnnBody
             | ACond AnnExpr AnnExpr AnnExpr
             | AAssign Expr AnnExpr
               deriving (Eq, Show)

data AnnDef = ADef1 Expr AnnExpr
            | ADef2 Expr [Expr] AnnBody
            | ADef3 [AnnDef]
              deriving (Eq, Show)

data AnnBody = ABody [AnnDef] [AnnExpr]
               deriving (Eq, Show)

data AnnCommOrDef = AComm AnnExpr | ADef AnnDef deriving (Eq, Show)

parExpr :: Parser Expr
parExpr = parVar 
        <|> try parLit 
        <|> try parCall 
        <|> try parLambda 
        <|> try parCond
        <|> parAssign 

parLit :: Parser Expr
parLit = fmap Literal (try $ 
           do
              x <- parToken
              if lit x then return (conv x) else empty
         ) <|> unexpected "Not a literal"
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
                                then return (Var s)
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
                    <|> (fmap Def $ token parDef)

definedVars :: Def -> S.Set String
definedVars (Def1 (Var s) _) = S.singleton s
definedVars (Def2 (Var s) _ _) = S.singleton s
definedVars (Def3 ds) = foldMap definedVars ds

freeInDef :: Def -> S.Set String
freeInDef (Def1 _ e) = freeVarsEx e
freeInDef (Def2 _ es b) = freeVars b `S.difference` foldMap freeVarsEx es
freeInDef (Def3 es) = foldMap freeInDef es

freeVarsEx :: Expr -> S.Set String
freeVarsEx (Var s) = S.singleton s
freeVarsEx (Literal _) = S.empty
freeVarsEx (Call a b) = S.union (freeVarsEx a) (foldMap freeVarsEx b)
freeVarsEx (Lambda vars body) = S.difference (freeVars body) 
                                  (foldMap freeVarsEx vars)
freeVarsEx (Cond x y z) = foldMap freeVarsEx [x, y, z]
freeVarsEx (Assign a b) = S.union (freeVarsEx a) (freeVarsEx b)

freeVars :: Body -> S.Set String
freeVars (Body defs exprs) = S.union (foldMap freeVarsEx exprs)
                                   (foldMap freeInDef defs)
                             `S.difference` foldMap definedVars defs

topDefined :: [CommOrDef] -> VarT
topDefined xs = foldr helper (M.fromList []) (map getVars xs)
  where
    helper ::S.Set String -> M.Map String Int -> M.Map String Int
    helper s m = M.union m (M.fromList $ zip (S.toList s) [0,0..])
    getVars :: CommOrDef -> S.Set String
    getVars (Comm _) = S.empty
    getVars (Def d) = definedVars d

annotateEx :: VarT -> Expr -> AnnExpr
annotateEx t (Var s) = AVar s t
annotateEx _ (Literal x) = ALiteral x
annotateEx t (Call f xs) = ACall (annotateEx t f) (map (annotateEx t) xs)
annotateEx t (Lambda es b) = ALambda es (annotateBody newt b)
  where
    newt = M.unionWith min 
            ((+1) <$> t) 
            (M.fromList $ zip (S.toList $ foldMap freeVarsEx es) [0,0..])  
annotateEx t (Cond a b c) = ACond (annotateEx t a) (annotateEx t b)
                                                   (annotateEx t c)
annotateEx t (Assign a b) = AAssign a (annotateEx t b)

annotateBody :: VarT -> Body -> AnnBody
annotateBody t (Body ds es) = ABody (map (annotateDef newt) ds) 
                                    (map (annotateEx newt) es)
  where
    newt = M.unionWith min t
            (M.fromList $ zip (S.toList $ foldMap definedVars ds) [0,0..])

annotateDef :: VarT -> Def -> AnnDef
annotateDef t (Def1 a b) = ADef1 a (annotateEx t b)
annotateDef t (Def2 a b c) = ADef2 a b (annotateBody newt c)
  where
    newt = M.unionWith min 
             ((+1) <$> t)
             (M.fromList $ zip (S.toList $ foldMap freeVarsEx b) [0,0..])
annotateDef t (Def3 ds) = ADef3 $ map (annotateDef newt) ds
  where
    newt = M.unionWith min t
            (M.fromList $ zip (S.toList $ foldMap definedVars ds) [0,0..])

annotateProgram :: VarT -> [CommOrDef] -> [AnnCommOrDef]
annotateProgram t xs = map (ann newt) xs
  where
    ann t (Def d) = ADef $ annotateDef t d
    ann t (Comm e) = AComm $ annotateEx t e
    newt = M.unionWith min t (topDefined xs)

allVars :: [CommOrDef] -> S.Set String
allVars = foldMap getFree
  where
    getFree (Comm x) = freeVarsEx x
    getFree (Def x) = freeInDef x