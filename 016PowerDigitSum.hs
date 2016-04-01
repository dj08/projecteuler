{-
Power digit sum
Problem 16
215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 21000? -}
import Data.Char
sumOfDigits :: Integer -> Int
sumOfDigits k = sum . map (digitToInt) $ show k

ans = sumOfDigits (2^1000)

-- Better still - no libraries
ans1 = sum $ map ord $ show $ 2^1000
