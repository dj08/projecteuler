{-
Permuted multiples
Problem 52
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits. -}

ans :: Integer
ans = take 1 (filter isPermutedDigits [1..])

isPermutedDigits :: Integer -> Bool
isPermutedDigits x
  | (length (show x)) /= (length (show (6*x))) = False
  | srtDigs (show x) /= srtDigs (show (6*x)) = False
  | srtDigs (show x) /= srtDigs (show (6*x)) = False










