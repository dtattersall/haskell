-- divides evenly into a range is really just a "find the lcm" for a set of numbers.
-- It turns out haskell already has an lcm function. In addition, lcm is associative...

findlcm [] = 1
findlcm (x:xs) = lcm x (findlcm xs)

answer = findlcm [1..20]
