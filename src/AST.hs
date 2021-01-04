module AST where

import Relude hiding (Type, TVar)

data Term
  -- | Literals (a.k.a primitives)
  = Lit Literal 
  -- | Constants
  | 
  -- | Variables
  | Var VarIndex
  -- | Abstraction of variables in terms (using de Bruine indices)
  -- For example \x : Int. x
  | Lam Term Term
  -- | Quantification (types of functions from values of type to values of type)
  -- For example. the term (\a : *.\x : a.x) : Pi * (Pi 0 0) 
  | Pi Term Term
  deriving (Show)

data Literal
  = BoolP Bool
  deriving (Show)

newtype VarIndex = VarIndex { _varIndex :: Natural }
  deriving (Show)

class Context x m a where
  Pushed :: x -> m a -> Context m a
  Lookup :: Integral x -> Context m x

makeSem 'Context
