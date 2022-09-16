
areEqual:: [Int] -> Bool
areEqual [] = error "!"
areEqual [x] = error "!"
areEqual (x:xs) = [] == [y | y <- xs, y /= x]