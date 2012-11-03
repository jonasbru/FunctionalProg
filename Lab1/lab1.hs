-- Functional Programming -- Lab Assignment 1 
-- Michael Fagno && Jonas Bru
-- The file was run through hlint without warnings.

import Data.List
import Test.QuickCheck


power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


------------ Part 1 ------------
steps :: Integer -> Integer -> Integer
steps _ k = k + 1

-- Always k + 1 steps. 


------------ Part 2 ------------
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product(genericReplicate k n)


------------ Part 3 ------------
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
		   | otherwise = n * power2 n (k-1) 


------------ Part 4 ------------
--             A
-- n > 0   => Normal case
-- n < 0   => Negative inputs
-- n == 0  => Special case, that should return 0
-- k == 0  => Special case, that should return 1
-- even k  => Because of power2
-- odd k   => Because of power2

--             B
prop_powers n k = 
	(x == power1 n k2) && (x == power2 n k2)
	where k2 = abs k;  --  avoid the errors with the negative k
		  x = power n k2		

--             C
--With list comprehension
testAll = and [prop_powers n k | n <- [0, 10, -10], k <- [0, 5, 6]]

-- Without
{- 
testAll = (prop_powers 10 5) 
	&& (prop_powers (-10) 5) 
	&& (prop_powers 10 0) 
	&& (prop_powers 0 5) 
	&& (prop_powers 10 0)
	&& (prop_powers 10 6)
	&& (prop_powers 10 7)
-}
	
--             D
testQC = quickCheck prop_powers

