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
  | SchemyProcedure Procedure
  | SchemyForm SchemyExp [SchemyExp]

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm exp exps) = "(SchemyForm "++ (show exp) ++ " " ++ (show exps) ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"

instance Eq SchemyExp where
  (SchemyBool a) == (SchemyBool b) = a == b
  (SchemyNumber a) == (SchemyNumber b) = a == b
  (SchemySymbol a) == (SchemySymbol b) = a == b

type SchemyEnv = Map String SchemyExp
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

eval:: SchemyEnv -> SchemyExp -> SchemyExp
eval _ x@(SchemyNumber _) = x
eval env x@(SchemyAdd _ _) = (SchemyNumber (evalNumber env x))
eval env x@(SchemyMult _ _) = (SchemyNumber (evalNumber env x))
eval env (SchemyLT x y) = (SchemyBool ((evalNumber env x) <= (evalNumber env y)))
eval env (SchemySymbol x) = env ! x
eval env x@(SchemyBool _) = x
eval env x@(SchemyNot _) = (SchemyBool (evalBool env x))
eval env x@(SchemyAnd _ _) = (SchemyBool (evalBool env x))
eval env (SchemyEquals x y) = (SchemyBool ((eval env x ) == (eval env y)))
eval env x@(SchemyProcedure _) = x
eval env (SchemyForm p xs) = (evalProcedure env p) env (map (eval env) xs)
-- Forma de hacer que no explote cuando no conoce la string
-- eval (SchemySymbol x) env = findWithDefault 0 x env

evalNumber:: SchemyEnv -> SchemyExp -> Double
evalNumber env (SchemyAdd x y) = (evalNumber env x ) + (evalNumber env y)
evalNumber env (SchemyMult x y) = (evalNumber env x ) * (evalNumber env y)
evalNumber env (SchemySymbol x) = evalNumber env (env ! x)
evalNumber env (SchemyNumber x) = x


evalBool:: SchemyEnv -> SchemyExp -> Bool
evalBool env (SchemyNot x) = not (evalBool env x)
evalBool env (SchemyAnd x y) = (evalBool env x ) && (evalBool env y)
evalBool env (SchemySymbol x) = evalBool env (env ! x)
evalBool env (SchemyBool x) = x

evalProcedure:: SchemyEnv -> SchemyExp -> Procedure
evalProcedure env (SchemyProcedure x) = x
evalProcedure env (SchemySymbol y) = evalProcedure env (env ! y)


subProc:: SchemyEnv -> [SchemyExp] -> SchemyExp
subProc _ [SchemyNumber x1, SchemyNumber x2] = SchemyNumber (x1-x2)
subProc env (SchemyNumber x: SchemyNumber y:xs) = subProc env (SchemyNumber (x-y):xs)

divProc:: SchemyEnv -> [SchemyExp] -> SchemyExp
divProc _ [SchemyNumber x1, SchemyNumber x2] = SchemyNumber (x1/x2)
divProc env (SchemyNumber x: SchemyNumber y:xs) = divProc env (SchemyNumber (x/y):xs)

lteProc:: SchemyEnv -> [SchemyExp] -> SchemyExp
lteProc _ [SchemyBool x1, SchemyBool x2] = SchemyBool (x1<x2)
lteProc env (SchemyBool x: SchemyBool y:xs) = lteProc env (SchemyBool (x<y):xs)

orProc:: SchemyEnv -> [SchemyExp] -> SchemyExp
orProc _ [SchemyBool x1, SchemyBool x2] = SchemyBool (x1||x2)
orProc env (SchemyBool x: SchemyBool y:xs) = orProc env (SchemyBool (x||y):xs)
