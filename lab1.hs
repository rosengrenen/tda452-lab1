import Test.QuickCheck
{- Lab 1, 2019
   Authors: Dino Pasalic, Rasmus Rosengren
   Lab group: Group 6
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- B -------------------------
power1 :: Integer -> Integer -> Integer
power1 n k = product [n | _ <- [1..k] ]

-- C -------------------------
power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | even k    = power2 (n * n) (div k 2)
           | otherwise = n * (power2 n (k - 1))

-- D -------------------------
{- 

<Describe your test cases here>
TEST CASE: Test if anything to the power of 0 results in 1
REASON: To make sure that anything to the power of results in 1, which is generally our base case

TEST CASE: Test if a negative base with an even exponent results in a positive result
REASON: To make sure that the exponentiation of a negative base works properly
   
TEST CASE: Test if a negative base with an odd exponent results in a negative result
REASON: To make sure that the exponentiation of a negative base works properly

TEST CASE: Test if a base and exponent that are greater than 1 give the correct result
REASON: To make sure that the functions follow the basic principles of exponentiation
-}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k) == (power1 n k) && (power n k) == (power2 n k)

--
powerTest :: Bool
powerTest = and [prop_powers n k | (n, k) <- [(0, 0), (-7, 3), (-7, 6), (7, 9)]] 

--
prop_powers' n k = prop_powers n (abs k)
