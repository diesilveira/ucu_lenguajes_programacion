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


eval:: (Map String Double) -> SchemyExp -> SchemyExp
eval _ (SchemyNumber x) = x
eval env (SchemyAdd x y) = (eval env x ) + (eval env y)
eval env (SchemyMult x y) = (eval env x) * (eval env y)
eval env (SchemySymbol x) = env ! x
eval env (SchemyBool x) = x
eval env (SchemyNot x) = not (eval env x) 
eval env (SchemyAnd x y) = (eval env x ) && (eval env y)
eval env (SchemyEquals x y) = (eval env x ) == (eval env y)
eval env (SchemyLT x y) = (eval env x ) <= (eval env y)
-- Forma de hacer que no explote cuando no conoce la string
-- eval (SchemySymbol x) env = findWithDefault 0 x env

evalNumber:: (Map String Double) -> SchemyExp -> SchemyExp


evalBool:: (Map String Double) -> SchemyExp -> SchemyExp
