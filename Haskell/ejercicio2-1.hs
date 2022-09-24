import Data.Map.Strict (Map, fromList, findWithDefault, (!))

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  | SchemyBool Bool
  | SchemyNot SchemyExp
  | SchemyEquals SchemyExp SchemyExp
  | SchemyLT SchemyExp SchemyExp
  | SchemyAnd SchemyExp SchemyExp
  deriving (Eq, Show)


eval:: (Map String SchemyExp) -> SchemyExp -> SchemyExp
eval _ x@(SchemyNumber _) = x
eval env x@(SchemyAdd _ _) = (SchemyNumber (evalNumber env x))
eval env x@(SchemyMult _ _) = (SchemyNumber (evalNumber env x))
eval env x@(SchemyLT _ _) = (SchemyNumber (evalNumber env x))
eval env (SchemySymbol x) = env ! x
eval env x@(SchemyBool _) = x
eval env x@(SchemyNot _) = (SchemyBool (evalBool env x))
eval env x@(SchemyAnd _ _) = (SchemyBool (evalBool env x))
eval env (SchemyEquals x y) = (SchemyBool ((eval env x ) == (eval env y)))
-- Forma de hacer que no explote cuando no conoce la string
-- eval (SchemySymbol x) env = findWithDefault 0 x env

evalNumber:: (Map String SchemyExp) -> SchemyExp -> Double
evalNumber env (SchemyAdd x y) = (evalNumber env x ) + (evalNumber env y)
evalNumber env (SchemyMult x y) = (evalNumber env x ) * (evalNumber env y)
evalNumber env (SchemyNumber x) = x


evalBool:: (Map String SchemyExp) -> SchemyExp -> Bool
evalBool env (SchemyNot x) = not (evalBool env x)
evalBool env (SchemyAnd x y) = (evalBool env x ) && (evalBool env y)
evalBool env (SchemyBool x) = x
