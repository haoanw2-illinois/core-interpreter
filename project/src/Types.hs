-- | Here are the types for the Core interpreter.
-- You may add constructors if you wish, but do not modify any that are already here.

module Types where

import qualified Data.HashMap.Lazy as M

-- NEW: runtime values
-- in Types.hs
data Value
  = VInt Int
  | VClosure [Name] Expr Env
  | VPack Int [Value]          -- fully constructed value: tag + fields
  | VConstr Int Int [Value]    -- constructor in-progress: tag, arity, captured args
  deriving (Eq, Show)

type Env = M.HashMap Name Value


type Name = String
type IsRec = Bool

data Expr = EVar Name                  -- Variables
          | ENum Int                   -- Numbers
          | EPack Int Int              -- Constructors
          | EAp Expr Expr              -- Applications
          | ELet
               IsRec                   -- is the let recursive?
               [(Name, Expr)]          -- Local Variable definitions
               Expr                    -- Body of the let
          | ECase                      -- Case Expressions
               Expr
               [(Int, [Name], Expr)]   -- Alternatives
          | ELam [Name] Expr           -- Functions (Lambdas)
   deriving (Eq, Show)

type Decl = (Name, [Name], Expr)       -- The name, parameter list, and body of a supercombinator declaration

type Core = M.HashMap Name Decl        -- A core program is an environment of declarations
