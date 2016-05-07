-- Brute force
-- first two elements are spurious as per PE, should be fine since they do not matter
bruteFibonacci :: [Integer]
bruteFibonacci = 1 : 2 : zipWith (+) bruteFibonacci (tail bruteFibonacci)

sumFibValuesBelow :: Integer -> Integer
sumFibValuesBelow x = sum (filter even (takeWhile (<= x) bruteFibonacci))

getRatio :: [Integer] -> [Double]
getRatio [x] = []
getRatio [] = []
getRatio (x1:x2:xs) = ((fromIntegral x2) / (fromIntegral x1)) : (getRatio xs)

goldenRatio = getRatio bruteFibonacci

-- isFibonacci :: (Eq a, Num a) => a -> Bool
-- isFibonacci n = mod 5*n^2 1 == 0
-- isFibonacci k | mod 5*k^2 1 == 0 = True
--               | otherwise        = False

-- Every third term is even, and every third term beautifully follows:
-- fib n = 4*fib n-3 + fib n-6
evenFibs = 2 : 8 : zipWith (+) (map (4*) (tail evenFibs)) evenFibs

-- bruteSumEvenFibsBelow :: Integer -> Integer
-- bruteSumEvenFibsBelow n = sum (takeWhile (<= n) evenFibs)

-- Here's my good one! Sum of difference equations:
-- evenFibs are: e(n) = 4*e(n-1) + e(n-2)
-- [there4]:4e(n)   = e(n+1) - e(n-1)
--          4e(n-1) = e(n)   - e(n-2)
--          4e(n-2) = e(n-1) - e(n-3)
--          ...
--          4e(3)   = e(4)   - e(2)
--          4e(2)   = e(3)   - e(1)
--          4e(1)   = e(2)   - e(0)
--         -------------------------------
-- Total: 4([sum] e(k) - e(0)) = e(n+1) + e(n) - e(1) - e(0)
-- => [sum] e(k) = (e(n+1) + e(n) - e(1) + 3e(0))/4 = 1089154 for
-- first 10 terms

sumEvenFibsBelow :: Int -> Int
sumEvenFibsBelow n = ((last $ take (x+1) evenFibs) +
                      (last $ take x evenFibs) -
                      8 + 6) `div` 4
  where x = length (takeWhile (<= n) evenFibs)
