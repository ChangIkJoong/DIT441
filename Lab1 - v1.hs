{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : --
Stability   : --

Authors     : --
Lab group   : --
-}


-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.

power :: Int -> Int -> Int
power n k
  | k < 0 = error "power: negative argument"
power n 0 = 1

power n k = n * power n (k-1)
x = 2

-- Part A ----------------------------------------------------------------------
-- stepsPower k gives the number of multiplications executed by power n k

stepsPower :: Int -> Int
stepsPower k
  | k < 0 = error "stepsPower: negative argument"
stepsPower k = k - 1

-- Part B ----------------------------------------------------------------------

power1 :: Int -> Int -> Int
power1 n k
  | k < 0 = error "power1: negative argument"
power1 n 0 = 1
power1 n k = product(replicate k n)



-- Part C ----------------------------------------------------------------------

power2 :: Int -> Int -> Int
power2 n k
  | k < 0 = error "power2: negative argument"
power2 n 0 = 1 

power2 n k
  | even k = power2 (n*n) (k `div` 2)
  | odd k = n * power2 n (k - 1)
  | k == 0 = 1



-- Part D ----------------------------------------------------------------------
{- 
1. does the same n k value give the same output? if yes: true. if no: false.

2. testing all iterations for [0..z] if yes: true. if no: false.

3.*FAILED* error does not return any value, simply terminates the program... 
the thought was to test the negative numbers, if returned true then this means the 
functions work in conjunction with each other generating the same response.
Including it here simply for an attempt: 
-- test3 = ([power n k | n <-[(-10)..0], k <-[(-10)..0]])
-}


test1::Int-> Int-> Bool
test1 n k = power n k == power1 n k 
        && power1 n k == power2 n k
         && power n k == power2 n k

test2::Int-> Bool
test2 z = and([power n k == power1 n k | n <-[0..z], k <-[0..z]]
        ++[power1 n k == power2 n k | n <-[0..z], k <-[0..z]]
        ++[power n k == power2 n k | n <-[0..z], k <-[0..z]])



testList n k z= [test1 n k, test2 z]

comparePower1:: Int -> Int -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2:: Int -> Int -> Bool
comparePower2 n k = power n k == power2 n k

compareAll z
    | and([comparePower1 n k == comparePower2 n k | n <-[0..z], k <-[0..z]] ++ testList z z z) == True = "True: Functions are correct."
    | otherwise = error "CompareAll: wrong input or debug code."





----------------------------------------------------------------------
{- 
  power n k == power1 n k
-- && power1 n k == power2 n k
-- && power n k == power2 n k

compareAll n k
    | and([comparePower1_2 n k] ++ testList n k 100) == True = "Functions are correct."
    | otherwise = error "CompareAll: wrong input or debug code."

comparePower1_2::Int-> Int-> Bool
comparePower1_2 n k= comparePower1 n k == comparePower2 n k


compareAll n k = and([comparePower1_2 n k] ++ testList n k 100)
  then "Functions are correct."
else : error "CompareAll: wrong input or debug code."


--testList = [test1 n k | (n, k)  <- [0..k]]

--testList:: [Bool]

--powertest n k = [n | x <- [1..k], comparePower12] 

--compareAll n k = True = "All functions are 'True', which means they are correct"

--compareAll:: [List]-> [List] -> Bool
--compareAll n k = comparePower1 n k == comparePower2 n k

--testList2 = [test1, test2, test3]++compareAll

--testAll:: Int -> Int -> Bool
--testAll n k = comparePower1 n k == comparePower2 n k

--http://zvon.org/other/haskell/Outputprelude/index.html

https://dev.to/awwsmm/relearn-you-a-haskell-part-2-list-comprehensions-tuples-and-types-g29

--http://zvon.org/other/haskell/Outputprelude/replicate_f.html

--power1 n k = product [n | x <- [1..k]] 
--https://stackoverflow.com/questions/27051421/replicate-function-code-in-haskell

-}