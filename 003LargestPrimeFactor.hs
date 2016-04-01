-- Largest prime factor
-- Problem 3
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

isPrime n = foldr (&&) True (map (==0) (map (mod n) [1..(n `div` 2)]))

brutePrimeFactorsOf n = [x | x <- [1..n], n `mod` x == 0]

{- Algo:
For input number n:
if 1,2 or 3: largest prime factor of n is n
if even: get largest prime factor of n/2
if odd:  get n/m = (q,r), m <- [3,5..(n+1)/2]
          if r == 0, get largest prime of q
          else, next
-}

-- largestPrime 1 = 1
-- largestPrime 2 = 2
-- largestPrime 3 = 3
-- largestPrime n | mod n 2 == 0   = largestPrime (div n 2)
--                | otherwise      | foldr (||) False (map (==0) (map (mod n) [3,5..(div (n+1) 2)])) == False = n
--                                 | otherwise  = last primeL                                                                     

--                -- | mod n m == 0 = largestPrime (div n m)
--                -- | otherwise    = largestPrime (div n (m+2))
--                --     where m = div (n+1) 2
betterPrimeFactorsOf :: Int -> [Int]
betterPrimeFactorsOf n = [x | x <- [1,3..ceiling(sqrt n)], div n x == 0]
 
