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


reserved :: [String]
reserved = ["let","in","letrec","rec","case","of","_"]


id :: Parser Name
id = try $ do
  first <- oneOf ['a' .. 'z']
  rest  <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")
  spaces
  let name = first:rest
  -- IMPORTANT: fail softly so other alternatives (like eLet) can match
  guard (name `notElem` reserved)
  return name
     
--- ### Lexicals
expr :: Parser Expr
expr =  try eCase
    <|> try eLet
    <|> eOr

symbol :: String -> Parser String
symbol s = string s <* spaces

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

eInt :: Parser Expr
eInt = do
  i <- int
  return (ENum i)

-- | Parse a variable reference
eVar :: Parser Expr
eVar = do
  name <- id
  return (EVar name)

-- | Parse arithmetic expressions
eArith :: Parser Expr
eArith = chainl1 eTerm op
  where
    op = (symbol "+" >> return (EAp . EAp (EVar "+")))
         <|> (symbol "-" >> return (EAp . EAp (EVar "-")))
         <|> (symbol "*" >> return (EAp . EAp (EVar "*")))
         <|> (symbol "/" >> return (EAp . EAp (EVar "/")))

-- | Parse terms (numbers or variables)
-- | Parse terms
eTerm :: Parser Expr
eTerm = eApp

-- | Parse comparison expressions
eComp :: Parser Expr
eComp = chainl1 eArith op
  where
    op = (try (symbol "<=") >> return (EAp . EAp (EVar "<=")))
         <|> (try (symbol ">=") >> return (EAp . EAp (EVar ">=")))
         <|> (try (symbol "==") >> return (EAp . EAp (EVar "==")))
         <|> (try (symbol "~=") >> return (EAp . EAp (EVar "/=")))  -- <-- add this line
         <|> (try (symbol "!=") >> return (EAp . EAp (EVar "/=")))
         <|> (try (symbol "/=") >> return (EAp . EAp (EVar "/=")))
         <|> (symbol "<" >> return (EAp . EAp (EVar "<")))
         <|> (symbol ">" >> return (EAp . EAp (EVar ">")))

-- | Parse logical AND expressions
eAnd :: Parser Expr
eAnd = chainl1 eComp op
  where
    op = (symbol "&" >> return (EAp . EAp (EVar "&")))

-- | Parse logical OR expressions
eOr :: Parser Expr
eOr = chainl1 eAnd op
  where
    op = (symbol "|" >> return (EAp . EAp (EVar "|")))


decl :: Parser Decl
decl = do name <- id
          params <- many id
          _ <- symbol "="
          body <- expr
          return (name,params,body)

-- | Skip data/type declarations (lines with ::=)
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

-- | Parse the whole program, skipping data/type declarations
core :: Parser Core
core = do
  many (try skipDataDecl)
  decls <- sepEndBy decl (symbol ";")   -- was: decl `sepBy` (symbol ";")
  return $ M.fromList [(n,v) | v@(n,_,_) <- decls]

parseCore :: String -> Either ParseError Core
parseCore text = parse (core <* eof) "Core" text

-- Add a parens helper
parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

-- Atoms: int | var | (expr)
eAtom :: Parser Expr
eAtom =  try eInt
     <|> eVar
     <|> parens expr

-- Application: left-assoc, e.g. f a b -> EAp (EAp (EVar "f") a) b
eApp :: Parser Expr
eApp = do
  f  <- eAtom
  xs <- many eAtom
  return (foldl EAp f xs)


-- let/letrec parsing
letBinding :: Parser (Name, Expr)
letBinding = do
  name <- id
  _ <- symbol "="
  rhs <- expr
  return (name, rhs)

eLet :: Parser Expr
eLet = do
  -- accept either "letrec" or "let [rec]"
  isRec <-  (try (symbol "letrec") >> return True)
        <|> (symbol "let" >> option False (symbol "rec" >> return True))
  defs <- letBinding `sepBy1` symbol ";"
  _ <- symbol "in"
  body <- expr
  return (ELet isRec defs body)


-- pattern: integer literal or "_"
pCasePat :: Parser (Int, [Name])   -- (tag, binders)
pCasePat =
      ((\n -> (n, [])) <$> int)
  <|> (symbol "_" >> return (-1, []))    -- default

-- single alternative: <pat> "->" expr
pAlt :: Parser (Int, [Name], Expr)
pAlt = do
  (tag, bs) <- pCasePat
  _         <- symbol "->"
  rhs       <- expr
  return (tag, bs, rhs)

-- case expr of alt ; alt ; ...
eCase :: Parser Expr
eCase = do
  _     <- symbol "case"
  scrut <- expr
  _     <- symbol "of"
  alts  <- sepEndBy1 pAlt (symbol ";")
  return (ECase scrut alts)

