{- 10001st prime
Problem 7
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number? -}

-- no brainer
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]
              
isPrime :: Int -> Bool
isPrime k = k > 1 && length (factors k) == 2

primeNumsList = [ x | x <- [1..], isPrime x == True ]

-- better: Divide only till sqrt n
factors' :: Integer -> [Integer]
factors' n = [ x | x <- [1..(ceiling . sqrt. fromInteger) n], n `mod` x == 0 ]
              
isPrime' :: Integer -> Bool
isPrime' k = k > 1 && length (factors' k) == 1

primeNumsList' = [ x | x <- [1..], isPrime' x == True ]

-- even better: Divide only by odds till sqrt n
oddFactors n = [ x | x <- [1,3..( ceiling . sqrt . fromInteger ) n],
                 n `mod` x == 0 ]

isEven n = n /= 2 && n `rem` 2 == 0

isPrime'' :: Integer -> Bool
isPrime'' k = isEven k == False &&
              length (oddFactors k) == 1

primeNumsList'' = [ x | x <- [1..], isPrime'' x == True ]

-- much better: divide only till prime numbers less than sqrt n
-- sadly, this lands into infinite recursion.
-- would work if we can figure out a way to feed it the list of prime numbers
-- and optimize hence.


