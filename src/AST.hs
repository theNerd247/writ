module AST where

import Relude hiding (Type, TVar, show)
import Control.Monad.Free (Free (..))
import Data.Functor.Foldable.TH
import Data.Functor.Foldable (cata, ghylo, distPara, distCata, distFutu, embed)
import Data.Functor.Identity (runIdentity)
import GHC.Show

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

instance (Show a) => Show (TermF a) where
  show (LitF l)   = show l 
  show (VarF i)   = show i
  show (LamF a b) = "(Lam (" <> show a <> ") (" <> show b <> "))" 
  show (AppF f a) = "(App (" <> show f <> ") (" <> show a <> "))"
  show (PiF a b)  = "(Pi  (" <> show a <> ") (" <> show b <> "))" 

data TypeCheckError a
  = BoxHasNoType
  | InvalidRuleFor a a
  | IsNotPiType a
  | IsNotLitType a
  | VarNotFound Natural
  | ReconstructionNotImplemented
  | ArgTypeDoesNotMatch a a
  | WithTyping (TypeCheckError a) (Typing a)
  | TypePolymorphismNotSupported
  deriving (Show)

-- | Functor containing language used for type-inference algorith
data Typing term
  = LitType Literal
  | Lookup Natural
  | ExtendCtx term (Typing term) 
  | LamType term (Typing term) 
  | PTSSelect (Typing term) (Typing term) 
  | WHNFKind (Typing term) 
  | ResTypeOfEquivArgPiType (Typing term) (Typing term) 
  | Substitute term (Typing term) 
  | AlreadyTyped term
  deriving (Show)

makeBaseFunctor ''Typing

data Ctx = Extend 
  { elem :: Term
  , env :: Ctx
  }
  | EmptyCtx

makeBaseFunctor ''Ctx

typeCheck :: Term -> Either (TypeCheckError Term) Term
typeCheck = typeCheck' EmptyCtx

typeCheck' :: Ctx -> Term -> Either (TypeCheckError Term) Term
typeCheck' ctx term = ghylo distPara distFutu (debugging fromTyping) toTyping term ctx

debugging :: (x ~ (t -> Either (TypeCheckError a) r))
  => (TypingF a b1 -> x)
  -> TypingF a (Typing a, b1) -> x
debugging alg x ctx = case alg (snd <$> x) ctx of
  Left (WithTyping err _) 
    -> Left $ WithTyping err $ embed $ fst <$> x
  Left err 
    -> Left $ WithTyping err $ embed $ fst <$> x
  Right r  
    -> Right r

fromTyping :: (r ~ (Ctx -> Either (TypeCheckError Term) Term)) => TypingF Term r -> r
fromTyping (AlreadyTypedF term)
  = pure . pure $ term
fromTyping (LamTypeF argType bodyType)
  -- | TODO: I feel this is a hack and there should be a way to rid us of using a recursive
  -- call within the algebra side of the hylo morphism.
  = \ctx -> do 
    b <- bodyType ctx
    let p = Pi argType b
    typeCheck' ctx p >> return p
fromTyping (LitTypeF l)
  = pure $ Lit <$> typeOfLit l
fromTyping (LookupF i)
  = findVar i
fromTyping (ExtendCtxF elem typ)
  = typ . (Extend elem) 
fromTyping (PTSSelectF sType tType)
  = join <$> (liftA2 . liftA2) pureTypeSysSelector sType tType 
fromTyping (WHNFKindF typ)
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
  = LamTypeF argType $ Free $ ExtendCtxF argType $ Pure bodyExpr
toTyping (App fExpr aExpr)
  = SubstituteF aExpr $ Free $ ResTypeOfEquivArgPiTypeF (Pure aExpr) $ Free $ WHNFKindF (Pure fExpr)
toTyping (Pi argType resType)
  = PTSSelectF (Free $ WHNFKindF $ Pure argType) (Free $ WHNFKindF $ Free $ ExtendCtxF argType $ Pure resType)

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
pureTypeSysSelector (Lit Box)  (Lit Star) = Left $ TypePolymorphismNotSupported
pureTypeSysSelector (Lit Box) (Lit Box)   = return . Lit $ Box
pureTypeSysSelector x y = Left $ InvalidRuleFor x y
