
areEqual:: Eq a => [a] -> Bool
areEqual [] = error "!"
areEqual [x] = error "!"
areEqual (x:xs) = null [y | y <- xs, y /= x]