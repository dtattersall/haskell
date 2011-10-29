isNotPalindrome :: Show a => a -> Bool
isNotPalindrome x = (show x) /= reverse (show x)

processlst :: Int -> [Int] -> Int
processlst currmax productlst =
    let palindromelst = (dropWhile (isNotPalindrome) (takeWhile (>currmax) productlst))
    in (if length palindromelst == 0 then currmax else max currmax (head palindromelst))

startlst = reverse [100..999]
mainlst = map (\x -> (map (x*) startlst)) startlst
answer = foldl processlst 0 mainlst
