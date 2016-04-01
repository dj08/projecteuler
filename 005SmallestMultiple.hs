{-
Smallest multiple
Problem 5
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? -}

-- Basically find a minimal set from [1..x], that divides all k!, k <= x

{- Usefule lambda functions...
> filter (\x -> mod 10 x == 0) [1..10]
[1,2,5,10]

Or clever currying...
> filter ((==0) . mod 10) [1..10]
[1,2,5,10]

whatever you like... :) -}

-- no brainer
foldr lcm 1 [1..20]
-- 232792560... haha!
