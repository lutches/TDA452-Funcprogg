{- Lab 1
   Date: 2/11-2022
   Authors: Lukas Åkefeldt & Jennifer Hallberg
   Lab group: Group 11
 -}
--------------------------------------------
module Lab1 where 
import Test.QuickCheck
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1



-- B -------------------------
-- power1
power1:: Integer->Integer->Integer
power1 n k
   | k < 0 = error "power: negative argument"
power1 n k = product([n| x<-[1..k]])



-- C -------------------------
-- power2
power2:: Integer->Integer->Integer
power2 n k
   | k == 0 = 1
   | k < 0 = error "power: negative argument"
   | odd k = n * power2 n (k - 1)
   | otherwise = power2 (n*n) (div k 2)


-- D -------------------------
{- 
<Describe your test cases here>
well behaved testcases
(0, 1)
(2, 2)
(10, 100)
(5, 0)
(-5, 4)
have picked these numbers as i felt it nessesary to check zero, a odd number,
an even number and a large number. ^0 and n that is negative

undefined testcases
(10, (-1)) 
i have choosen this test as it's the power of a negative number. and that does not work yet.

 -}

-- 
prop_powers n k = (power n k == power1 n k) == (power1 n k == power2 n k)

--
powerTest = and[prop_powers (fst x) (snd x)| x<- list]
   where list = [(0,1),(2,2),(10,100),(5,0),((-5),4)]

--
prop_powers' n k = prop_powers n (abs k)
