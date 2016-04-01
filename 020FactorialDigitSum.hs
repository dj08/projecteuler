{-
Factorial digit sum
Problem 20
n! means n × (n - 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100! -}

import Data.Char
-- fact :: (Num a, Integral a) => a -> a
fact n = foldr (*) 1 [1..n]

sumOfDigits :: Integer -> Integer
sumOfDigits k
  | (restOfDigits k) == 0 = lastDigit k
  | otherwise             = (lastDigit k) + (sumOfDigits (restOfDigits k))
          where restOfDigits m = div m 10
                lastDigit m = mod m 10

ans = sumOfDigits (fact 100)

-- Much smaller code (take fact definition from above)
digitsList :: Integer -> String
digitsList k = show k

sumOfDigitsList :: String -> Int
sumOfDigitsList k = sum $ map (digitToInt) k

ans1 = sumOfDigitsList $ digitsList $ fact 100

-- Even smaller
ans2 = sum [digitToInt x | x <- show (product [1..100])]
