{-
Summation of Primes
Problem 9

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million. -}

primesBelowN :: Integer -> [Integer]
primesBelowN n = 2:3:filter f [6*k+i | k <- [1..(n-1)`div`6], i <- [-1, 1]]
                     where f x = foldr g True [2..truncate(sqrt(fromInteger x))]
                                 where g t ac = (x `rem` t /= 0) && ac

ans :: Integer
ans = sum $ primesBelowN 2000000

main = print $ sum $ primesBelowN 2000000
