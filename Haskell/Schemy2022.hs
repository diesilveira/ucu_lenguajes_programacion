module Schemy2022 where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Map.Strict (Map, fromList, (!))
import Text.Read (readMaybe)

type SchemyEnv = Map String SchemyExp

type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

data SchemyExp
  = SchemyBool Bool
  | SchemyNumber Double
  | SchemyProcedure Procedure
  | SchemySymbol String
  | SchemyForm SchemyExp [SchemyExp]

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm f args) = "(SchemyForm "++ (show f) ++" "++ (show args) ++")"

-- Semantics -------------------------------------------------------------------

eval :: SchemyEnv -> SchemyExp -> SchemyExp
-- eval _ _ = error "eval is not implemented!" -- Please implement it.
eval env (SchemySymbol s) = env ! s
eval env (SchemyForm f args) = (evalProcedure env f) env args
eval env v = v

evalBool env exp = case eval env exp of
  SchemyBool b -> b
  _ -> error ("Expected boolean for "++ (show exp) ++"!")
evalNumber env exp = case eval env exp of
  SchemyNumber n -> n
  _ -> error ("Expected number for "++ (show exp) ++"!")
evalProcedure env exp = case eval env exp of
  SchemyProcedure p -> p
  _ -> error ("Expected procedure for "++ (show exp) ++"!")

aor :: Procedure -> Procedure
aor p env args = p env (map (eval env) args) 

procAdd :: Procedure
procAdd = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyNumber (n1 + n2))

procEq :: Procedure
procEq = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyBool (n1 == n2))

procAnd :: Procedure
procAnd = aor (\_ [SchemyBool b1, SchemyBool b2] -> SchemyBool (b1 && b2))

basicEnv :: SchemyEnv
basicEnv = fromList [
  ("pi", SchemyNumber pi),
  ("+", SchemyProcedure procAdd),
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
    putStrLn (show (parse line)) -- Show the parse result
    -- putStrLn (unparse (parse line)) -- Echo the code
    -- putStrLn (unparse (eval basicEnv (parse line))) -- Print the evaluation
    repl
  else
    return ()