-- CISC 360 a1, Fall 2021

module A1 where

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id =  20152682

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE


-- Q2.1: between

-- between m n p == True if and only if n is strictly between m and p
-- (between 1 3 4 should return True; between 10 300 300 should return False
--  because 300 is not *strictly* less than 300)
--
between :: Integer -> Integer -> Integer -> Bool
between = (\m -> (\n -> (\p -> if m < n && n < p then True else False )))

-- Testing between:
--
-- Test cases for between
-- CISC 360 CONVENTION: A variable beginning with "test_" should evaluate to True.
-- So test_between1 should be True because  between 1 2 3  should evaluate to True.
-- test_between2 should be True because  not (between 1 0 5)  should evaluate to True,
-- which will be the case when  between 1 0 5  evaluates to False.
test_between1, test_between2, test_between3, test_between4 :: Bool
test_between1 = between 1 2 3
test_between2 = not (between 1 0 5)
test_between3 = not (between (-2) (-2) (-1))
test_between4 = not (between 35 30 10)
-- Do all tests together
test_between :: Bool
test_between = test_between1 && test_between2
                             && test_between3
                             && test_between4

-- Q2.2: parity
--
-- Given two nonnegative integers m, n:
-- parity m n  returns  1  if exactly one of m and n is an odd number,
--                      0  otherwise
--
-- Hint: mod k 2 returns 1 if k is odd, 0 otherwise.
--
parity :: Integer -> Integer -> Integer
parity = \m ->( \n -> ( if (mod m 2 == 1 && mod n 2 == 0) || (mod m 2 == 0 && mod n 2 == 1) then 1 else 0 ))

-- Testing parity:
-- 
test_parity1 = (parity 40 5) == 1
test_parity2 = (parity 9 13) == 0
test_parity3 = (parity 20 20) == 0
test_parity4 = (parity 13 9) == 0
test_parity = test_parity1 && test_parity2 && test_parity3 && test_parity4

{-
Stepping questions

Q3.1. Replace the underlines (_______).

   expression                   justification

   (\z -> z - (3 + z)) 10
=> 10 - (3 + 10)    Replace bound variable (z) with argument (10)
=> 10 - 13          complete arithmetic in brackets
=> -3               by arithmetic


Q3.2.  Replace the underlines (_______).
   Assume a function `ten' has been defined:

   ten :: Integer -> Integer
   ten x = 10 * x

     expression                            justification

     (\q -> (\y -> q 5)) ten 2
  => (\y -> ten 5) 2                    by function application
  => ten 5                              by function application
  => 10*5                               by function application
  => 50                                 by arithmetic     
-}

{-
Q4.
  Write a function `spiral' that, given a pair of numbers `span' and `dir',
  returns 1 if `span' equals 0,
  and otherwise returns (span * dir) * spiral (span - 1, 0 - dir).
-}

spiral :: (Integer, Integer) -> Integer
spiral (span, dir) = if span == 0 then 1 else (span * dir) * spiral (span - 1, 0 - dir)
  
-- Testing spiral:
test_spiral1, test_spiral2, test_spiral3, test_spiral4 :: Bool
stest_spiral1 = (spiral (0, 36)  == 1)
test_spiral2 = (spiral (0, -36) == 1)
test_spiral3 = (spiral (5, 10)  == 12000000)
test_spiral4 = (spiral (7, -1)  == 5040)
test_spiral  = test_spiral1 && test_spiral2 && test_spiral3 && test_spiral4

-- Q5: 
--
-- spiral_seq n == string containing results of spiral (n, 1) for 0, ..., n
--                  separated by commas
--
-- For example,  spiral_seq 2  should return  "1,1,-2"
--   because spiral (0, 1) returns 1,
--           spiral (1, 1) returns 1,
--       and spiral (2, 1) returns -2.
--
-- Hints:
--    1. The built-in function  show  converts an integer to its representation as a string.
--
--    2. You can use the built-in function  ++  to concatenate strings.
--         For example, "10" ++ "," == "10,".
--
--    3. You may need to define a helper function for spiral_seq to call.

spiral_seq :: Integer -> [Char]
spiral_seq n = if n>=1 then spiral_seq(n-1) ++ "," ++ show(spiral(n,1)) else show(spiral(n,1))
