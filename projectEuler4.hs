findPalindrome :: (Integral t) => [t] -> [t] -> t
findPalindrome left right
    | left == [] || right == []  = -1
    | isPalindrome(llast*rlast)  = llast*rlast
    | llast > rlast              = findPalindrome (init left) right
    | otherwise                  = findPalindrome left (init right)
    where 
        llast = last left
        rlast = last right
        isPalindrome x = (show x) == reverse (show x)

answer = findPalindrome [100..999] [100..999]
