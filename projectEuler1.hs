answer = foldl (\x y -> if (((mod y 3) == 0) || ((mod y 5) == 0)) then x + y else x) 0 [1..1000]
