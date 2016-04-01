{-
Longest Collatz sequence
Problem 14
The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million. -}

-- no brainer
-- collatzSequenceFor :: (Num [a], Integral a) => a -> [a]
collatzSequence n
  | n == 1    = [1]
  | even n    = n:collatzSequence (div n 2)
  | otherwise = n:collatzSequence (3*n + 1)

-- map (length . collatzSequence)

-- brainer
{-
The shortest sequence can come for powers of 2: any 2^n number
will generate a sequence of exactly n terms. No jumps, just a
monotonic decline for any 2^n.

Further, any odd term will become *even* by Collatz sequence transform:
3n+1 (surely even for any odd n).

Hence, n/2 is the next transform for sure.

So, the longest sequence should jump between odd and even terms.
Let's see this case:

Last number is bound to be 1. Only 2 can give this.
For 2: only 4.
For 4: only 8.
For 8: only 16 (other solutions do not return positive integers).

So, let's backpolate the series, there isn't any choice left with
the computer to generate the longest one. -}

-- This function returns the possible parent Collatz term, given the last term
possibleCollatzTerms :: Integer -> [Integer]
possibleCollatzTerms k
  | k == 1           = [2]
  | mod (k-1) 3 == 0 = [(div (k-1) 3), 2*k]
  | otherwise        = [2*k]

-- This function ensures that our current possibile parents are legitimiate,
-- that is, always within the 1 million limit
-- terminateSeq :: 
terminateSeq (x:xs)
  | (even x) && (x <= 2000000) = x : terminateSeq xs -- x is legitimate
  | (odd x)  && (x <= 1000000) = x : terminateSeq xs -- x is legitimate
  | otherwise                  = terminateSeq xs -- discard x, is beyond limits

-- while (length (terminateSeq sol) /= 1)
  
