-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Data.Functor.Identity
import Control.Monad
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

-- ======= Lexicals & basics =======

symbol :: String -> Parser String
symbol s = string s <* spaces

int :: Parser Int
int = do
  digits <- many1 digit <?> "an integer"
  spaces
  return (read digits :: Int)

reserved :: [String]
reserved = ["let","in","letrec","rec","case","of","_"]

id :: Parser Name
id = try $ do
  first <- oneOf ['a' .. 'z']
  rest  <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
  spaces
  let name = first:rest
  guard (name `notElem` reserved)  -- soft-fail so other alts (e.g., eLet/eCase) can match
  return name

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

arrow :: Parser String
arrow = symbol "->"

-- ======= Expressions (precedence: case/let lowest, then |, &, comps, arith, app, atoms) =======

expr :: Parser Expr
expr =  try eCase
    <|> try eLet
    <|> eOr

-- OR
eOr :: Parser Expr
eOr = chainl1 eAnd (symbol "|" >> return (EAp . EAp (EVar "|")))

-- AND
eAnd :: Parser Expr
eAnd = chainl1 eComp (symbol "&" >> return (EAp . EAp (EVar "&")))

-- Comparisons
eComp :: Parser Expr
eComp = chainl1 eArith op
  where
    op =  (try (symbol "<=") >> return (EAp . EAp (EVar "<=")))
      <|> (try (symbol ">=") >> return (EAp . EAp (EVar ">=")))
      <|> (try (symbol "==") >> return (EAp . EAp (EVar "==")))
      <|> (try (symbol "~=") >> return (EAp . EAp (EVar "/=")))
      <|> (try (symbol "!=") >> return (EAp . EAp (EVar "/=")))
      <|> (try (symbol "/=") >> return (EAp . EAp (EVar "/=")))
      <|> (symbol "<"        >> return (EAp . EAp (EVar "<")))
      <|> (symbol ">"        >> return (EAp . EAp (EVar ">")))

-- Arithmetic
eArith :: Parser Expr
eArith = chainl1 eTerm op
  where
    op =  (symbol "+" >> return (EAp . EAp (EVar "+")))
      <|> (symbol "-" >> return (EAp . EAp (EVar "-")))
      <|> (symbol "*" >> return (EAp . EAp (EVar "*")))
      <|> (symbol "/" >> return (EAp . EAp (EVar "/")))

-- Application (left-assoc over atoms)
eTerm :: Parser Expr
eTerm = eApp

eApp :: Parser Expr
eApp = do
  f  <- eAtom
  xs <- many eAtom
  return (foldl EAp f xs)

-- ======= Atoms: int | lambda | Pack{t,n} | var | (expr) =======

eInt :: Parser Expr
eInt = ENum <$> int

eLam :: Parser Expr
eLam = do
  _      <- symbol "\\"
  params <- many1 id
  _      <- arrow
  body   <- expr
  return (ELam params body)

pPack :: Parser Expr
pPack = do
  _ <- symbol "Pack"
  _ <- symbol "{"
  t <- int
  _ <- symbol ","
  n <- int
  _ <- symbol "}"
  return (EPack t n)

eVar :: Parser Expr
eVar = EVar <$> id

eAtom :: Parser Expr
eAtom =  try eInt
     <|> try eLam
     <|> try pPack
     <|> eVar
     <|> parens expr

-- ======= let / letrec =======

letBinding :: Parser (Name, Expr)
letBinding = do
  name <- id
  _    <- symbol "="
  rhs  <- expr
  return (name, rhs)

eLet :: Parser Expr
eLet = do
  isRec <-  (try (symbol "letrec") >> return True)
        <|> (symbol "let" >> option False (symbol "rec" >> return True))
  defs <- letBinding `sepBy1` symbol ";"
  _    <- symbol "in"
  body <- expr
  return (ELet isRec defs body)

-- ======= case expressions =======

-- pattern: "_" (default) OR integer tag with optional binders (for constructors)
-- For Int scrutinee, binders should be empty; runtime enforces that.
pCasePat :: Parser (Int, [Name])   -- (tag, binders)
pCasePat =
      (symbol "_" >> return (-1, []))
  <|> do
        t  <- int
        bs <- many id
        return (t, bs)

pAlt :: Parser (Int, [Name], Expr)
pAlt = do
  (tag, bs) <- pCasePat
  _         <- arrow
  rhs       <- expr
  return (tag, bs, rhs)

eCase :: Parser Expr
eCase = do
  _     <- symbol "case"
  scrut <- expr
  _     <- symbol "of"
  alts  <- sepEndBy1 pAlt (symbol ";")
  return (ECase scrut alts)

-- ======= Top-level decls & program =======

decl :: Parser Decl
decl = do
  name   <- id
  params <- many id
  _      <- symbol "="
  body   <- expr
  return (name, params, body)

-- Skip data/type declarations (lines with ::=)
skipDataDecl :: Parser ()
skipDataDecl = do
  _ <- id
  optional (symbol "*")
  spaces
  _ <- string "::="
  skipMany (noneOf ";")
  _ <- char ';'
  spaces
  return ()

core :: Parser Core
core = do
  many (try skipDataDecl)
  decls <- sepEndBy decl (symbol ";")
  return $ M.fromList [(n,v) | v@(n,_,_) <- decls]

parseCore :: String -> Either ParseError Core
parseCore text = parse (core <* eof) "Core" text
