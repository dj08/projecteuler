{-
Sum square difference
Problem 6
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum. -}

-- no brainer
sumOfSquares xs = sum (map (\x -> x*x) xs)
squareOfSum  xs = (sum xs) ^ 2
differenceSoS xs = (sqaureOfSum xs) - sumOfSquares xs
ans = differenceSoS [1..100]

-- try better
{-
squareOfSum - sumOfSquares == sumOfCubes - sunOfSquares

