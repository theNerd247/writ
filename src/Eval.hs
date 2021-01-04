module Eval where

import Relude hiding (Type, TVar, on, id, (.))
import AST
import Control.Lens
import Control.Category
import Polysemy
import Polysemy.Error

data Value 
  = VLam (Value -> Value)
  | Syntax Expr

beta :: Expr -> Expr
beta = sem >=> reify

sem :: Expr -> Value
sem (App (Lam _ e1) e2)  
  = do 
      (VLam f) <- sem e1 
      f <$> sem e2
sem (Lam _ e) 
  = VLam <$> (reify >=> sem . App e)
sem x
  = Syntax x

reify :: Value -> Expr
reify = undefined
