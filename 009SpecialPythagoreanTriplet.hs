{-
Special Pythagorean triplet
Problem 9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc. -}

-- no brainer
triplets = [(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
specialTriplets = [(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b], a+b+c == 1000, a^2 + b^2 == c^2]

tripletProd (x,y,z) = x*y*z

ans = tripletProd $ head $ specialTriplets
