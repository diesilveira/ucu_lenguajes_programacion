import Data.Map (Map, fromList, toList)
import Data.List (intercalate)

-- ejercicio 1.1
areEqual:: Eq a => [a] -> Bool
areEqual [] = error "!"
areEqual [x] = error "!"
areEqual (x:xs) = null [y | y <- xs, y /= x]

-- ejercicio 1.2
data Json =  JSONObject (Map String Json) | JSONList [Json] | JSONNull
                | JSONBool Bool | JSONString String
                | JSONDouble Double deriving(Eq,Show)

stringify:: Json -> String
stringify (JSONObject xs) ="{" ++ intercalate "," (map stringifyValue (toList xs)) ++ "}"
    where
        stringifyValue (key,value) = "\"" ++ key ++ "\": " ++ stringify value
stringify (JSONList xs) = "[" ++ intercalate "," (map stringify xs) ++ "]"
stringify (JSONString x) = x
stringify (JSONBool x) = show x
stringify (JSONDouble x) = show x
stringify JSONNull = "null"

{---------------------------
line to test this:
let test01 = JSONObject (fromList [("x", JSONDouble 1.0),("y", JSONString "true"), ("list", JSONList [JSONNull])])
putStrLn (stringify test01)
----------------------------}
