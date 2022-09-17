import Data.Map (Map, fromList, toList)
import Data.List (intercalate)

-- ejercicio 1.1
areEqual:: Eq a => [a] -> Bool
areEqual [] = error "!"
areEqual [x] = error "!"
areEqual (x:xs) = null [y | y <- xs, y /= x]

-- ejercicio 1.2
data Json =  JsonObject (Map String Json) | JsonList [Json] | JsonNull
                | JsonBool Bool | JsonString String 
                | JsonDouble Double deriving(Eq,Show)

stringify:: Json -> String
stringify (JsonObject xs) ="{" ++ (intercalate "," (map stringifyValue (toList xs))) ++ "}"
    where
        stringifyValue (key,value) = "\"" ++ key ++ "\": " ++ (stringify value) 
stringify (JsonList xs) = "[" ++ (intercalate "," (map stringify xs)) ++ "]"
stringify (JsonString x) = x
stringify (JsonBool x) = show x
stringify (JsonDouble x) = show x
stringify JsonNull = "null"
