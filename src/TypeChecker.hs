module TypeChecker where

import AST

data TypeCheckerError 
  = ArgTypeNotStar
  | ApplierIsNotALambda
  | ApplierIsNotPiType
  | InvalidFunctionTypeArg
  | InvalidFunctionTypeRes
  | KindMismatch
  | TypeMismatch
  deriving (Show)

inferType :: Members '[Error TypeCheckerError, Context Term, Context Type] r => Term -> Sem r Type
inferType (Prim (BoolP _))   
  = PrimT BoolT
inferType (Var (VarIndex i)) 
  = Lookup @_ @Term i
inferType (Lam ty tm)      
  = inferKind ty >>= \case
    Star -> TFun ty <$> (Pushed ty $ inferType tm)
    _    -> Throw ArgTypeNotStar
inferType (App t1 t2)
  = do 
    t1' <- inferType t1 
    t2' <- inferType t2
    case t1' of 
      TFun t11 t12 -> 
        case t11 `typeEq` t2' 
          True  -> return t12 
          False -> TypeMismatch
      _ -> Throw ApplierIsNotALambda
inferType (PiT k t)
  = Pushed k $ (Pi k) <$> inferType t 
inferType (AppT tm ty) 
  = do 
    t1' <- inferType tm 
    k2' <- inferKind ty
    case t1' of 
      Pi k11 ty12 -> 
        case k11 == k2' 
          True  -> 
          False -> KindMismatch
      _ -> Throw ApplierIsNotPiType


inferKind :: Members '[Error TypeCheckerError, Context Type] r => Type -> Sem r Kind
inferKind (PrimT _) 
  = return $ Star
inferKind (TVar (VarIndex i)) 
  = Lookup i
inferKind (TFun t1 t2) 
  = do
    k1' <- inferKind t1
    k2' <- inferKind t2
    case (k1', k2') of
      (Star, Star) -> return Star
      (_, Star)    -> Throw InvalidFunctionTypeArg
      _            -> Throw InvalidFunctionTypeRes
inferKind (T
