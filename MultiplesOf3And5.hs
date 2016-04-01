{-
Multiples of 3 and 5
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

numsBelow10 :: [Int]
numsBelow10 = [1..10]

-- Old solution
multiplesOf3And5' :: (Integral a) => [a] -> [a]
multiplesOf3And5' inpList = map isDivisibleby3Or5 inpList
    where isDivisibleby3Or5 num | rem num 3 == 0 = num
                                | rem num 5 == 0 = num
                                | otherwise      = 0

-- Using awesome lists
multiplesOf3And5 :: (Integral a) => [a] -> [a]
multiplesOf3And5 xs =
  [x | x <- init xs, (mod x 3 == 0) || (mod x 5 == 0)]

-- sum all of these
sumMultiples xs = sum (multiplesOf3And5 xs)

-- Better solution?
-- This one lessens the loading on the computer.
-- However, it assumes that you start from 0 - cannot act on a range...
threesBelow :: Int -> [Int]
threesBelow n = [0,3..n-1]

fivesBelow :: Int -> [Int]
fivesBelow n = [0,5..n-1]

fifteensBelow :: Int -> [Int]
fifteensBelow n = [0,15..n-1]

sumMultiplesBelow :: Int -> Int
sumMultiplesBelow n =
  sum (threesBelow (n-1)) + sum (fivesBelow (n-1)) - sum (fifteensBelow (n-1))

-- Still bad... how about some better methods?
sumThreesBelow :: Integer -> Integer
sumThreesBelow n = 3 * greatestThree * (greatestThree + 1) `div` 2
  where greatestThree = (n-1) `div` 3

sumFivesBelow :: Integer -> Integer
sumFivesBelow n = 5 * greatestFive * (greatestFive + 1) `div` 2
  where greatestFive = (n-1) `div` 5

sumFifteensBelow :: Integer -> Integer
sumFifteensBelow n = 15 * greatestFifteen * (greatestFifteen + 1) `div` 2
  where greatestFifteen = (n-1) `div` 15

sumMultiplesBelow' :: Integer -> Integer
sumMultiplesBelow' n =
  (sumThreesBelow n) + (sumFivesBelow n) - (sumFifteensBelow n)

-- More concise
sumMultiplesOfNBelowX :: Integer -> Integer -> Integer
sumMultiplesOfNBelowX x n = n * ((x-1) `div` n) * (((x-1) `div` n)+ 1) `div` 2

multiplesCount :: Integer -> Integer -> Integer
multiplesCount p q = p `div` q

conciseSumMultiplesOf3And5Below :: Integer -> Integer
conciseSumMultiplesOf3And5Below k =
  (foldr (+) 0 (map (sumMultiplesOfNBelowX k) [3,5])) - (sumMultiplesOfNBelowX k 15)

