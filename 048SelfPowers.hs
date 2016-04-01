{-
Self powers
Problem 48
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000. -}

-- no brainer
selfPowers xs = sum (map (\x -> x^x) xs)
-- show $ (\str -> drop (length str - 10) str ) $ show $ selfPowers [1..1000]

-- better
betterSum = sum [n^n | n <- [1..1000]] `mod` 10^10

-- even better (for manual computation, but slower
-- otherwise...)
evenBetterSum = sum [n^n | n <- [1..1000], n `rem` 10 /= 0 ] `mod` 10^10

-- main = print $ (\str -> drop (length str - 10) str ) $ show $ selfPowers [1..1000]
