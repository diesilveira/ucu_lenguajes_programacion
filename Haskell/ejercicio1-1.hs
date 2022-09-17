import Data.Map (Map, fromList)

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
stringify JsonObject [(key, value):xs] = "{\"" ++ key ++ "\":" stringify(value) ++ stringify(xs)  ++ "}"
stringify JsonList [x] = stringify(x) ++ "]"
stringify JsonList [x:xs] = stringify(x) ++ "," stringify(xs)

stringify JsonString x = x
stringify JsonBool x = show x
stringify JsonDouble x = show x
stringify JsonNull = "null"
