module AST where

import Relude hiding (Type, TVar)
import Control.Monad.Free (Free (..))
import Data.Functor.Foldable.TH
import Data.Functor.Foldable (cata, ghylo, distCata, distFutu)
import Data.Functor.Identity (runIdentity)

data Literal
  -- | Values 
  = BoolV Bool
  -- | Types
  | BoolT
  -- | Constant for type of types (kinds)
  | Star
  -- | Constat for type of kinds
  | Box
  deriving (Show)

newtype VarIndex = VarIndex { _varIndex :: Natural }
  deriving (Show)

data Term 
  -- | Literals (a.k.a value and type primitives)
  = Lit Literal 
  -- | Variables
  | Var VarIndex
  -- | Abstraction of variables in terms (using de Bruine indices)
  -- For example \x : Int. x
  | Lam Term Term
  -- | Application of an abstraction
  | App Term Term
  -- | Quantification (types of functions from values of type to values of type)
  -- For example. the term (\a : *.\x : a.x) : Pi * (Pi 0 0) 
  | Pi Term Term
  deriving (Show)

makeBaseFunctor ''Term

data TypeCheckError a
  = BoxHasNoType
  | InvalidRuleFor a a
  | IsNotPiType a
  | IsNotLitType a
  | VarNotFound Natural
  | ReconstructionNotImplemented
  | ArgTypeDoesNotMatch a a
  deriving (Show)

-- | Functor containing language used for type-inference algorith
data Typing term
  = LitType Literal
  | Lookup Natural
  | ExtendCtx term (Typing term) 
  | Reconsruct (TermF (Typing term)) (Typing term) 
  | PTSSelect (Typing term) (Typing term) 
  | WHNType (Typing term) 
  | ResTypeOfEquivArgPiType (Typing term) (Typing term) 
  | Substitute term (Typing term) 
  | AlreadyTyped term

makeBaseFunctor ''Typing

data Ctx = Extend 
  { elem :: Term
  , env :: Ctx
  }
  | EmptyCtx

makeBaseFunctor ''Ctx

typeCheck :: Term -> Either (TypeCheckError Term) Term
typeCheck = ($EmptyCtx) . ghylo distCata distFutu (fromTyping . fmap runIdentity) toTyping

fromTyping :: (r ~ (Ctx -> Either (TypeCheckError Term) Term)) => TypingF Term r -> r
fromTyping (AlreadyTypedF term)
  = pure . pure $ term
fromTyping (ReconsructF termF typ)
  = (>>) <$> typ <*> reconstruct termF
fromTyping (LitTypeF l)
  = pure $ Lit <$> typeOfLit l
fromTyping (LookupF i)
  = findVar i
fromTyping (ExtendCtxF elem typ)
  = typ . (Extend elem) 
fromTyping (PTSSelectF sType tType)
  = join <$> (liftA2 . liftA2) pureTypeSysSelector sType tType 
fromTyping (WHNTypeF typ)
  = (fmap . fmap) toWHNF typ
fromTyping (ResTypeOfEquivArgPiTypeF argType resType)
  = join <$> (liftA2 . liftA2) isPiTypeAndArgsBetaEquiv argType resType
fromTyping (SubstituteF argTerm bodyTerm)
  = (fmap . fmap) (substitute argTerm) bodyTerm

findVar :: Natural -> Ctx -> Either (TypeCheckError Term) Term
findVar i = cata alg i
  where
    alg :: r ~ (Ctx -> Either (TypeCheckError Term) Term) => Maybe r -> r
    alg Nothing EmptyCtx   = Left $ VarNotFound i
    alg Nothing (Extend{..})  = Right elem
    alg (Just f) (Extend{..}) = f env
    alg (Just f) EmptyCtx     = f EmptyCtx

reconstruct :: r ~ (Ctx -> Either (TypeCheckError Term) Term) => TermF r -> r 
reconstruct (PiF argType resType) = (liftA2 . liftA2) Pi argType resType
reconstruct _ = pure $ Left $ ReconstructionNotImplemented

-- | TODO: implement 
toWHNF :: Term -> Term
toWHNF x = x

isPiTypeAndArgsBetaEquiv :: Term -> Term -> Either (TypeCheckError Term) Term
isPiTypeAndArgsBetaEquiv argType t@(Pi argType' resType)
  | argType `betaEquiv` argType' = Right resType
  | otherwise                    = Left $ ArgTypeDoesNotMatch argType t
isPiTypeAndArgsBetaEquiv _  t 
  = Left $ IsNotPiType t

-- | TODO: implement
betaEquiv :: Term -> Term -> Bool
betaEquiv _ _ = True

-- | TODO: implement
substitute :: Term -> Term -> Term
substitute _ bodyTerm = bodyTerm

toTyping :: Term -> TypingF Term (Free (TypingF Term) Term) 
toTyping (Lit l)
  = LitTypeF l
toTyping (Var (VarIndex i))
  = LookupF i
toTyping (Lam argType bodyExpr)
  = ReconsructF reconstructedPiType $ Free $ piTyping argType bodyTyping
    where
      reconstructedPiType = PiF (Free $ AlreadyTypedF argType) bodyTyping
      bodyTyping = Free $ ExtendCtxF argType $ Pure bodyExpr
toTyping (App fExpr aExpr)
  = SubstituteF aExpr $ Free $ ResTypeOfEquivArgPiTypeF (Pure aExpr) $ Free $ WHNTypeF (Pure fExpr)
toTyping (Pi argType resType)
  = piTyping argType $ Pure resType

piTyping :: a ~ (Free (TypingF Term) Term) => Term -> a -> TypingF Term a
piTyping argType resType =
  PTSSelectF (Free $ WHNTypeF $ Pure argType) (Free $ WHNTypeF $ Free $ ExtendCtxF argType resType)

typeOfLit :: Literal -> Either (TypeCheckError a) Literal
typeOfLit (BoolV _) 
  = return BoolT
typeOfLit BoolT 
  = return Star
typeOfLit Star
  = return Box
typeOfLit Box
  = Left $ BoxHasNoType
 
pureTypeSysSelector :: Term -> Term -> Either (TypeCheckError Term) Term
pureTypeSysSelector (Lit Star) (Lit Star) = return . Lit $ Star
pureTypeSysSelector (Lit Box) (Lit Box) = return . Lit $ Box
pureTypeSysSelector x y = Left $ InvalidRuleFor x y
