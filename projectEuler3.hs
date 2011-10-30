import Data.List

-- we note that given an integer x, if x is cleanly divisible by one of the prime numbers strictly < x, then x is composite, else x is prime
getPrimes :: Integral a => a -> [a] -> [a]
getPrimes current primeslst =
    let currentIsPrime = [] == dropWhile (\x -> 0 /= mod current x) primeslst
    in if currentIsPrime then current:(getPrimes (current+1) (primeslst ++ [current])) else getPrimes (current+1) primeslst
primes = getPrimes 2 []  -- Note: this is an infinite list of primes

-- the basic idea is to repeatedly divide the number into a prime factor and a smaller number, this will generate the prime factorization of 
-- the number, we then take the maximum of that to get the largest prime factor 
factorize :: Integer -> Maybe (Integer, Integer)
factorize 1 = Nothing
factorize number =
    let primeFactor = head (dropWhile (\x -> 0 /= mod number x) primes)
    in if primeFactor == number then Just(primeFactor, 1) else Just(primeFactor, div number primeFactor)

getLargestPrime :: Integer -> Integer
getLargestPrime number = maximum (unfoldr factorize number)
answer = getLargestPrime 600851475143
