{-
Almost equilateral triangles
Problem 94
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-}
-- Triangles like that would have sides n, n, n+1/n-1
dims = [n | n <- [1..333333333]]
-- 3.33e8 is max side length for above perimeter

-- Two cases of areas: x-1 side and x+1 side
-- areas :: (Floating a) => a -> [a]
areas x =
  ( 0.25*(x-1)*sqrt(((3*x)-1)(x+1)),0.25*(x+1)*sqrt(((3*x)+1)(x-1)))
