{-
Circular primes
Problem 35
The number, 197, is called a circular prime because all rotations of
the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
37, 71, 73, 79, and 97.

How many circular primes are there below one million?-}

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

-- A recursively created sieve... good performance
primesBelowN' n =
  2:3:myList (0, [6*k+i | k <- [1..(n-1)`div`6], i <- [-1, 1]])
      where
        g :: Integer -> Integer -> Bool
        g x y = y `rem` x /= 0
        myList :: ([Integer], [Integer]) -> [Integer]
        myList (sv, ys)
          | k <= sn   = myList (k:sv, filter (g k) ys)
          | otherwise = (reverse sv) ++ filter (g sn) ys 
              where k  = head ys
                    sn = squareRoot n

main = print $ sum $ primesBelowN' 2000000

