module Parser2 where

import Text.Trifecta
import Data.Char (isSpace, isAlpha, isAscii, isPrint)
import Data.List (intercalate)
import Control.Applicative ((<|>), empty, liftA2, liftA3)
import Data.List (nub)

data Token = Identifier String 
           | Boolean Bool
           | Number Double 
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
            boolConv "#f" = Boolean False
            boolConv "#t" = Boolean True
            numConv = Number . read
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
            lit (Boolean _) = True
            lit (Number _) = True
            lit (Character _) = True
            lit (String _) = True
            lit _ = False
            conv (Boolean x) = LitBool x
            conv (Number x) = LitNum x
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
      return (Def2 v formals body)))
  else (if x /= "begin" then empty else 
    do
      defs <- many (token parDef)
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

data CommOrDef = Comm Expr | Def Def deriving (Eq, Show)

parProgram :: Parser [CommOrDef]
parProgram = many $ try (fmap Comm $ token parExpr) 
                    <|> (fmap Def $ token parDef)