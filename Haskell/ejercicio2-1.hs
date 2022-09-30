import Data.Map.Strict (Map, fromList, findWithDefault, (!))
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Text.Read (readMaybe)

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

aor :: Procedure -> Procedure
aor p env args = p env (map (eval env) args) 

procAdd :: Procedure
procAdd = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyNumber (n1 + n2))

procEq :: Procedure
procEq = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyBool (n1 == n2))

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

procAnd :: Procedure
procAnd = aor (\_ [SchemyBool b1, SchemyBool b2] -> SchemyBool (b1 && b2))

basicEnv :: SchemyEnv
basicEnv = fromList [
  ("pi", SchemyNumber pi),
  ("+", SchemyProcedure procAdd),
  ("-", SchemyProcedure subProc),
  ("div", SchemyProcedure divProc),
  ("==", SchemyProcedure procEq),
  ("&&", SchemyProcedure procAnd)]

-- Syntax ----------------------------------------------------------------------

unparse :: SchemyExp -> String
unparse (SchemyBool b) = if b then "true" else "false"
unparse (SchemyNumber d) = show d
unparse (SchemySymbol s) = s
unparse (SchemyForm f args) = "("++ (intercalate " " (map unparse (f:args))) ++")"
unparse (SchemyProcedure _) = error "Cannot unparse procedures!"

mayParse :: String -> Maybe (SchemyExp, String)
mayParse input
  | input == "" = Nothing
  | isSpace (head input) = mayParse (tail input)
  | (head input) == '(' = mayParseForm [] (tail input)
  | otherwise = if (token == "true") then Just (SchemyBool True, rest)
    else if (token == "false") then Just (SchemyBool False, rest)
    else case readMaybe token of
      Just n -> Just (SchemyNumber n, rest)
      _ -> Just (SchemySymbol token, rest)
  where token = takeWhile (\s -> notElem s " ()\f\n\r\t") input
        rest = drop (length token) input

mayParseForm :: [SchemyExp] -> String -> Maybe (SchemyExp, String)
mayParseForm list input
  | input == "" = Nothing
  | isSpace (head input) = mayParseForm list (tail input)
  | (head input) == ')'  = Just (SchemyForm (head list) (tail list), (tail input))
  | otherwise = case mayParse input of
    Just (exp, rest) -> mayParseForm (list ++ [exp]) rest
    _ -> Nothing

parse :: String -> SchemyExp
parse input = case mayParse input of
  Just (exp, rest) | all isSpace rest -> exp
  _ -> error "Parse error!"

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Welcome to Schemy REPL. Input an blank line to exit."
  repl

repl :: IO ()
repl = do
  line <- getLine
  if not (all isSpace line) then do
    -- putStrLn (show (parse line)) -- Show the parse result
    -- putStrLn (unparse (parse line)) -- Echo the code
    putStrLn (unparse (eval basicEnv (parse line))) -- Print the evaluation
    repl
  else
    return ()