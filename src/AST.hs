module AST where

import Relude hiding (Type, TVar)
import Control.Lens
import Polysemy
import Polysemy.Error
import Control.Arrow ((***), Kleisli (..))

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

instance Eq Term where
  (==) = undefined

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

makeLenses ''VarIndex
makeWrapped ''VarIndex

data VarCtx x m a where
  Pushed :: x -> m a -> VarCtx x m a
  Lookup :: Natural -> VarCtx x m x

makeSem ''VarCtx

data TypeCheckError 
  = BoxHasNoType
  | InvalidRuleFor Literal Literal
  | IsNotPiType Term 
  | TypesNotEquivalent Term Term Term
  | IsNotLitType Term
  deriving (Show)

typeOf :: Members '[VarCtx Term, Error TypeCheckError] r => Term -> Sem r Term
typeOf (Lit x)     
  = Lit <$> typeOfLit x
typeOf (Var i)     
  = lookup $ i^._Wrapped
typeOf (App potentialLambda arg)
  = 
    isPiType potentialLambda 
    >>= uncurry (>>) . (argsTypeMatches arg *** substituteWith arg)
typeOf (Lam varType bodyExpr) 
  = (pushed varType $ typeOf bodyExpr) >>= constructPiType varType
typeOf (Pi argType resType) 
  = join $
    lambdaCubeSelection
    <$> (typeOf argType >>= isLitType) 
    <*> (pushed argType (typeOf resType) >>= isLitType)


substituteWith :: Members '[VarCtx Term] r => Term -> Term -> Sem r Term
substituteWith = undefined

constructPiType :: Members '[VarCtx Term, Error TypeCheckError] r => Term -> Term -> Sem r Term
constructPiType varType resType =
  typeOf p >> return p
    where 
      p = Pi varType resType

isPiType :: Members '[VarCtx Term, Error TypeCheckError] r => Term -> Sem r (Term, Term)
isPiType t = typeOf t >>= \case 
  Pi a b -> return (a,b)
  _      -> throw $ IsNotPiType t

isLitType :: Members '[VarCtx Term, Error TypeCheckError] r => Term -> Sem r Literal
isLitType (Lit x) = return $ x
isLitType x = throw $ IsNotLitType x

argsTypeMatches :: Members '[VarCtx Term, Error TypeCheckError] r => Term -> Term -> Sem r ()
argsTypeMatches arg _type = do
  argType <- typeOf arg 
  case argType == _type of
    True -> return ()
    False -> throw $ TypesNotEquivalent arg argType _type

typeOfLit :: Members '[VarCtx Term, Error TypeCheckError] r => Literal -> Sem r Literal
typeOfLit (BoolV _) 
  = return BoolT
typeOfLit BoolT 
  = return Star
typeOfLit Star
  = return Box
typeOfLit Box
  = throw BoxHasNoType

lambdaCubeSelection :: Members '[Error TypeCheckError] r => Literal -> Literal -> Sem r Term
-- enable abstraction at term label
lambdaCubeSelection Star Star = pure . Lit $ Star
-- enable abstraction at type level
lambdaCubeSelection Box Box = pure . Lit $ Box
lambdaCubeSelection x y = throw $ InvalidRuleFor x y
