import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sort)

{-
Summation of Primes
Problem 9

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million. -}

-- Integer square root
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

-- My own algo, near to but theoritically not as optimal as Sieve of
-- Eratosthenes. Takes 1.71s on my machine.
primesBelowN :: Integer -> [Integer]
primesBelowN n = 2:3:filter f [6*k+i | k <- [1..(n-1) `div` 6], i <- [-1, 1]]
                     where f x = foldr g True xs
                                 where g t ac = (x `rem` t /= 0) && ac
                                       xs = takeWhile (<= squareRoot x) $ tail $ primesBelowN n

-- A recursively created sieve... better performance now. Takes 1.238s! :)
primesBelowN' n =
  2:3:myList (0, [6*k+i | k <- [1..(n-1)`div`6], i <- [-1, 1]])
      where
        g :: Integer -> Integer -> Bool
        g x y = y `rem` x /= 0
        myList :: (Integer, [Integer]) -> [Integer]
        myList (sv, ys) 
          | k <= sn   = myList (k+sv, filter (g k) ys)
          | otherwise = [sv, sum (filter (g sn) ys)]
              where k  = head ys
                    sn = squareRoot n
-- Do this to create a sieve instead of doing the sum
--          myList :: ([Integer], [Integer]) -> [Integer]
--          | k <= sn   = myList (k:sv, filter (g k) ys)
--          | otherwise = (reverse sv) ++ filter (g sn) ys 

-- A way faster approach using stateful programming. Refer
-- https://programmingpraxis.files.wordpress.com/
-- 2012/09/programmingwithprimenumbers.pdf for details. This is not my algo,
-- just keeping here for future reference.
ancientSieve :: Int -> UArray Int Bool
ancientSieve n = runSTUArray $ do
    bits <- newArray (2, n) True
    forM_ [2 .. n] $ \p -> do
        isPrime <- readArray bits p
        when isPrime $ do
            forM_ [2*p, 3*p .. n] $ \i -> do
                writeArray bits i False
    return bits

primesBelowN'' :: Int -> [Int]
primesBelowN'' n = [p | (p, True) <-
                  assocs $ ancientSieve n]

main = print $ sum $ primesBelowN'' 2000000

{-

Fri, 3 May 2013, 23:44 
Lucy_Hedgehog   Development Team
Python  
Switzerland	
   Quote   Report    114
Here is a solution that is more efficient than the sieve of Eratosthenes. It is derived from similar algorithms for counting primes. The advantage is that there is no need to find all the primes to find their sum.

The main idea is as follows: Let S(v,m) be the sum of integers in the range 2..v that remain after sieving with all primes smaller or equal than m. That is S(v,m) is the sum of integers up to v that are either prime or the product of primes larger than m. 

S(v, p) is equal to S(v, p-1) if p is not prime or v is smaller than p*p. Otherwise (p prime, p*p<=v) S(v,p) can be computed from S(v,p-1) by finding the sum of integers that are removed while sieving with p. An integer is removed in this step if it is the product of p with another integer that has no divisor smaller than p. This can be expressed as

S(v,p)=S(v,p−1)−p(S(v/p,p−1)−S(p−1,p−1)).S(v,p)=S(v,p−1)−p(S(v/p,p−1)−S(p−1,p−1)).

Dynamic programming can be used to implement this. It is sufficient to compute S(v,p) for all positive integers v that are representable as floor(n/k) for some integer k and all p≤v√p≤v.
 
Python
Hide Code

def P10(n):
    r = int(n**0.5)
    assert r*r <= n and (r+1)**2 > n
    V = [n//i for i in range(1,r+1)]
    V += list(range(V[-1]-1,0,-1))
    S = {i:i*(i+1)//2-1 for i in V}
    for p in range(2,r+1):
        if S[p] > S[p-1]:  # p is prime
            sp = S[p-1]  # sum of primes smaller than p
            p2 = p*p
            for v in V:
                if v < p2: break
                S[v] -= p*(S[v//p] - sp)
    return S[n]

The complexity of this algorithm is about O(n0.75)O(n0.75) and needs 9 ms to find the solution. Computing the sum of primes up to different bounds n gives:
n = 2*10**7: 12272577818052  0.04 s
n = 2*10**8: 1075207199997334  0.2 s
n = 2*10**9: 95673602693282040  1 s
n = 2*10**10: 8617752113620426559  6.2 s
n = 2*10**11: 783964147695858014236  34 s
n = 2*10**12: 71904055278788602481894  3 min

I also have a C++ version of this algorithm. This one solves the problem in 700μμs. It needs 10 hours to compute the sum of primes up to 10171017 which is 129408626276669278966252031311350.

It is also possible to improve the complexity of the algorithm to O(n2/3)O(n2/3), but the code would be more complex.-}

{-
Apparently, this works as well with the right set of modules

import Math.NumberTheory.Primes
main = print . sum . takeWhile (<2000000) $ primes

Haven't measured the performance gain.-}
