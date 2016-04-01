{- Problem statement:
Largest palindrome product
Problem 4
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers. -}

-- no brainer
import Data.List
all3DigitPalindromeProducts   = sort [p*q | p <- [100..999], q <- [100..999], show (p*q) == reverse (show (p*q))]
all3DigitPalindromeProducts'  = sort [p*q | p <- [100..999], q <- [100..p], show (p*q) == reverse (show (p*q))]
all3DigitPalindromeProducts'' = sort [p*q | p <- [100..999], q <- [100..p], p*q < head all3DigitPalindromeProducts'', show (p*q) == reverse (show (p*q))]


-- [(p,q) | p = 100*a + 10*b + c, q = 100*d + 10*e + f, (e*c + b*f + c*f) == (a*e + b*d + a*f
