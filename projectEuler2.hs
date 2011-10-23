fiblist :: (Real t) => t -> [t] -> [t]
fiblist limit lst
    | length lst < 2  = fiblist limit [0,1]
    | first >= limit  = lst
    | otherwise       = fiblist limit (lst ++ [first + second])
       where 
         first = last lst
         second = last (init lst) 
answer = foldl (\x y -> if (even y) then x+y else x) 0 (fiblist 4000000 [])
