module FinalExam where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

-- if list is empty, aka no strings with length < 6, are not fully lowercase or are empty, then it return "zzzzz"
f :: [String] -> String
f xs = minimum $ "zzzzz":[x | x <- xs, length x < 6 && isLowercaseLC x && x /= ""] -- otherwise after filtering it takes the smallest string using minimum

isLowercaseLC :: String -> Bool
isLowercaseLC xs = and [ isLower x | x <- xs] -- helper function to check if all letter in string are lowercase. Also isLower '/' or '?' gives false, so it filters out non-letters

-- 1b

g :: [String] -> String
g [] = "zzzzz"
g (x:xs) | satisfies x = min x (g xs) -- chains min x (min x' (min x'' (min x''' "zzzzz")))
         | otherwise = g xs -- ignores x if it does not satisfy conditions

satisfies :: String -> Bool
satisfies l = length l < 6 && isLowercaseRec l && l /= ""

isLowercaseRec :: String -> Bool
isLowercaseRec "" = True
isLowercaseRec (x:xs) = isLower x && isLowercaseRec xs

-- 1c

h :: [String] -> String
h xs = foldr min "zzzzz" (filter (\x -> length x < 6 && isLowercaseMap x && x /= "") xs) -- folds the filtered list with min and "zzzzz" as 'base/final' case

isLowercaseMap :: String -> Bool
isLowercaseMap = foldr ((&&) . isLower) True

prop_fgh :: [String] -> Bool
prop_fgh xs = f xs == g xs && g xs == h xs && h xs == f xs
-- checks if all three functions give the same result
-- *FinalExam> quickCheck prop_fgh
-- +++ OK, passed 100 tests.

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i xs ys = tail xs ++ [head ys]

-- 2b

j :: [[a]] -> [[a]]
j xs = [i x y | (x, y) <- zip xs (tail xs)] ++ [i (last xs) (head xs)]
-- zip [1,2,3,4] [2,3,4] == [(1,2), (2,3), (3,4)] ++ (i 4 1) (this is not exact, just shows the idea)

-- 2c

k :: [[a]] -> [[a]]
k xs = mix xs
 where mix [x] = [i x (first xs)] -- adds the last mixed with first
       mix (x:y:xs) = i x y : mix (y:xs) -- mixes all the strings except the last one with the first one, kinda subs the zip part of function 'j'
       
       first [x] = x
       first (x:xs) = x


{-
prop_jk :: Eq a => [[a]] -> Property
prop_jk xs = xs /= [] && and [not (null x) | x <- xs] ==> 
             length (j xs) == length (k xs) 
             && length (k xs) == length xs
             && length xs == length (j xs)
-}
prop_jk :: Eq a => [[a]] -> Property
prop_jk xs = xs /= [] && and [not (null x) | x <- xs] ==> j xs == k xs
-- first discards all empty list cases, then checks if both recursive and LC functions give the same result
-- *FinalExam> quickCheck prop_jk
-- +++ OK, passed 100 tests; 98 discarded.

-- Question 3

data Wff = X
         | Y
         | Tr
         | Fa
         | Not Wff
         | Wff :&: Wff
         | Wff :|: Wff
         | Wff :->: Wff
  deriving (Eq, Show)

instance Arbitrary Wff where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa,
              liftM Not wff,
              liftM2 (:&:) wff wff,
              liftM2 (:|:) wff wff,
              liftM2 (:->:) wff wff]
      where
      wff = gen (n `div` 2)

-- I hate this part and am too tired to comment and quickCheck this section
-- 3a

eval :: Bool -> Bool -> Wff -> Bool
eval wf1 wf2 X = wf1
eval wf1 wf2 Y = wf2
eval wf1 wf2 (Not p) = not (eval wf1 wf2 p)

eval wf1 wf2 (_ :|: Tr) = True
eval wf1 wf2 (Tr :|: _) = True
eval wf1 wf2 (p :|: Fa) = eval wf1 wf2 p
eval wf1 wf2 (Fa :|: q) = eval wf1 wf2 q

eval wf1 wf2 (p :&: Tr) = eval wf1 wf2 p
eval wf1 wf2 (Tr :&: q) = eval wf1 wf2 q
eval wf1 wf2 (_ :&: Fa) = False
eval wf1 wf2 (Fa :&: _) = False

eval wf1 wf2 (p :|: q) = eval wf1 wf2 p || eval wf1 wf2 q
eval wf1 wf2 (p :&: q) = eval wf1 wf2 p && eval wf1 wf2 q
eval wf1 wf2 (p :->: q) = eval wf1 wf2 (Not p :|: q)

-- 3b

simple :: Wff -> Bool
simple Tr = True
simple Fa = True
simple (Not p) = simple p

simple (X :|: Y) = True
simple (Y :|: X) = True
simple (X :&: Y) = True
simple (Y :&: X) = True

simple (_ :|: Tr) = False
simple (_ :|: Fa) = False
simple (Tr :|: _) = False
simple (Fa :|: _) = False

simple (_ :&: Tr) = False
simple (_ :&: Fa) = False
simple (Tr :&: _) = False
simple (Fa :&: _) = False

simple (p :|: q) = simple p && simple q
simple (p :&: q) = simple p && simple q
simple (p :->: q) = simple (Not p :|: q)

-- 3c

simplify :: Wff -> Wff
simplify Tr = Tr
simplify Fa = Fa
simplify X = X
simplify Y = Y
simplify (Not Tr) = Fa
simplify (Not Fa) = Tr
simplify (Not (p :|: q)) = simplify (Not p) :|: simplify (Not q)
simplify (Not (p :&: q)) = simplify (Not p) :&: simplify (Not q)
simplify (Not p) = Not (simplify p)

simplify (Fa :&: p) = Fa
simplify (p :&: Fa) = Fa
simplify (Tr :&: p) = p
simplify (p :&: Tr) = p

simplify (Fa :|: p) = p
simplify (p :|: Fa) = p
simplify (Tr :|: p) = Tr
simplify (p :|: Tr) = Tr

simplify (Fa :->: p) = Tr
simplify (p :->: Tr) = Tr
simplify (Tr :->: p) = p
simplify (p :->: Fa) = Not p

simplify (p :|: q) = simplify p :|: simplify q
simplify (p :&: q) = simplify p :&: simplify q
simplify (p :->: q) = simplify' (simplify p :->: simplify q)
 where
   simplify' (Fa :->: p) = Tr
   simplify' (p :->: Tr) = Tr
   simplify' (Tr :->: p) = p
   simplify' (p :->: Fa) = Not p
   simplify' p = p
