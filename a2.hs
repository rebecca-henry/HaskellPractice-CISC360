-- CISC 360 a2, Fall 2021

-- SEE THE FILE a2.pdf
-- for instructions

module A2 where
import Data.Char

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 20152682

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.


{-
   Q2: rewrite
-}
divisible_by :: Int -> Char -> Bool
divisible_by factor ch = (mod (ord ch) factor == 0)

rewrite :: (Char -> Bool) -> String -> String
rewrite important []       = []
rewrite important (c : cs) = if important c==True then [c] ++ [c] ++ rewrite important(cs) else [c] ++ rewrite important(cs)

test_rewrite1 = (rewrite (divisible_by 2) "Queen's") == "Queenn's"
test_rewrite2 = (rewrite (\x -> x == ' ') "it's a deed" == "it's  a  deed")
test_rewrite3 = (rewrite (\c -> (c < 'a')) "sIlLy CaPiTaLiZaTiOn") == "sIIlLLy  CCaPPiTTaLLiZZaTTiOOn"


{-
  Q3: lists
-}

{-
  Q3a. Fill in the definition of listCompare.
  See a2.pdf for instructions.
-}
listCompare :: ([Integer], [Integer]) -> [Bool]

listCompare ([],   []  )   = []
listCompare (x:xs, y:ys)   = if x == y then [True] ++ listCompare(xs,ys) else [False]++listCompare(xs,ys)
listCompare (x:xs, []  )   = [False]++listCompare(xs,[])
listCompare ([],   y:ys)   = [False] ++ listCompare([],ys)

test_listCompare1 = listCompare ([1, 2, 4], [3, 2, 0]) == [False, True, False]
test_listCompare2 = listCompare ([1, 2, 1, 1], [1, 2]) == [True, True, False, False]
test_listCompare3 = listCompare ([1, 1], [1, 1, 1, 1]) == [True, True, False, False]


{-
  Q3b.
  Briefly explain why listCompare cannot
  be implemented by

    listCompare :: ([Integer], [Integer]) -> [Bool]
    listCompare (xs, ys) = zipWith (==) xs ys

  Write your brief explanation here:
  This implementation will not pad with False if the lists are of different lengths.
  
-}


{-
  Q3c. Fill in the definition of polyCompare.
  See a2.pdf for instructions.
-}
polyCompare :: (a -> a -> Bool) -> ([a], [a]) -> [Bool]
polyCompare eq ([],   []  )   = []
polyCompare eq (x:xs, y:ys)   = if [eq x y] == [True] then [True] ++ polyCompare eq(xs, ys) else [False] ++ polyCompare eq(xs, ys)
polyCompare eq (x:xs, []  )   = [False] ++ polyCompare eq(xs,[])
polyCompare eq ([],   y:ys)   = [False] ++ polyCompare eq([], ys)

test_polyCompare1 = polyCompare (\i -> \j -> i == j) ([1, 2, 4], [3, 2, 0])
                    == [False, True, False]

-- whoever calls polyCompare gets to define what "equal" means:
--  in test_polyCompare2, the definition of "equal" becomes whether two lists (here, strings) have the same length, regardless of the lists' contents
lengthsEqual :: [a] -> [a] -> Bool
lengthsEqual xs ys = (length xs == length ys)
test_polyCompare2 = polyCompare lengthsEqual (["a", "ab", "abcd"], ["ccc", "xy", ""])
                    == [False, True, False]


{-
  Q4. Songs
-}

data Song = Harmony Song Song
          | Melody Integer
          deriving (Show, Eq)    -- writing Eq here lets us use == to compare Songs
          
{-
  Q4a. sing: See a2.pdf for instructions.
-}
sing :: Song -> Song

sing (Harmony (Harmony(Melody n)(motif))(right)) = if n==0 then Harmony(Harmony(Melody n)(motif))(right) else motif
sing (Harmony(Harmony(Harmony(Melody 0)(s1))(s2))(s3)) = Harmony(Harmony(s1)(s3)) (Harmony(s2)(s3))
sing other = other

ascend = Harmony (Harmony (Melody 0) (Melody 1)) (Melody 2)
test_sing1 = (sing (Harmony ascend (Melody 3)))
              == Harmony (Harmony (Melody 1) (Melody 3)) (Harmony (Melody 2) (Melody 3))
test_sing2 = sing (sing (Harmony (Harmony (Melody 1) (Melody 3)) (Harmony (Melody 2) (Melody 3))))
              == Melody 3


{-
  Q4b. repeat_sing: See a2.pdf for instructions.
-}

repeat_sing :: Song -> Song
repeat_sing s = if sing s /= s then repeat_sing(sing s) else s

test_repeat1 = repeat_sing (Harmony ascend (Melody 3)) == Melody 3

{-
  Q4c. BONUS (worth a very small number of marks)
-}

-- diverging_song = undefined
