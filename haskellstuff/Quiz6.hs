module Quiz6(Set(MkSet),empty,unit,union,subset,set) where

import Data.List(nub,sort)
import Test.QuickCheck

data Set a = MkSet [a]

empty :: Ord a => Set a
empty =  MkSet []

unit :: Ord a => a -> Set a
unit x =  MkSet [x]

union :: Ord a => Set a -> Set a -> Set a
union (MkSet xs) (MkSet ys)  =  MkSet (merge xs ys)
  where
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] ys                   =  ys
  merge xs []                   =  xs
  merge (x:xs) (y:ys) | x < y   =  x : merge xs (y:ys)
                      | x == y  =  x : merge xs ys
                      | x > y   =  y : merge (x:xs) ys

intersect :: Ord a => Set a -> Set a -> Set a
intersect (MkSet xs) (MkSet ys)  =  MkSet (meet xs ys)
  where
  meet :: Ord a => [a] -> [a] -> [a]
  meet [] ys                   =  []
  meet xs []                   =  []
  meet (x:xs) (y:ys) | x < y   =  meet xs (y:ys)
                     | x == y  =  x : meet xs ys
                     | x > y   =  meet (x:xs) ys

subset :: Ord a => Set a -> Set a -> Bool
subset (MkSet xs) (MkSet ys)  =  sublist xs ys
  where
  sublist :: Ord a => [a] -> [a] -> Bool
  sublist [] ys                   =  True
  sublist (x:xs) []               =  False
  sublist (x:xs) (y:ys) | x < y   =  False
                        | x == y  =  sublist xs ys
                        | x > y   =  sublist (x:xs) ys

set :: Ord a => [a] -> Set a
set  =  foldr union empty . map unit

-- Testing

invariant :: Ord a => Set a -> Bool
invariant (MkSet xs) = ascending xs
  where
  ascending :: Ord a => [a] -> Bool
  ascending xs = and [ x < y | (x,y) <- zip xs (drop 1 xs) ]

list :: Set a -> [a]
list (MkSet xs)  =  xs

nubsort :: Ord a => [a] -> [a]
nubsort  =  nub . sort

prop_set :: [Int] -> Bool
prop_set xs  =
  invariant a  &&  list a == nubsort xs
  where
  a = set xs

prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys  =
  invariant (a `union` b) &&
    list (a `union` b) == nubsort (xs ++ ys)
  where
  a = set xs
  b = set ys

prop_intersect :: [Int] -> [Int] -> Bool
prop_intersect xs ys  =
  invariant (a `intersect` b) &&
    list (a `intersect` b) == nubsort [ x | x <- xs, x `elem` ys ]
  where
  a = set xs
  b = set ys

prop_subset :: [Int] -> [Int] -> Bool
prop_subset xs ys  =
  a `subset` b == and [ x `elem` ys | x <- xs ]
  where
  a = set xs
  b = set ys

prop_subset_pos :: [Int] -> [Int] -> [Int] -> Bool
prop_subset_pos xs ys zs  =
  (a `intersect` b) `subset` a &&
  (a `intersect` b) `subset` b &&
  a `subset` (a `union` b) &&
  b `subset` (a `union` b)
  where
  a = set (xs ++ zs)
  b = set (ys ++ zs)

prop_subset_neg :: [Int] -> [Int] -> [Int] -> Property
prop_subset_neg xs ys zs  =
  not (a `subset` b && b `subset` a) ==>
    not ((a `union` b) `subset` (a `intersect` b))
  where
  a = set (xs ++ zs)
  b = set (ys ++ zs)

main =
  quickCheck prop_set >>
  quickCheck prop_union >>
  quickCheck prop_intersect >>
  quickCheck prop_subset >>
  quickCheck prop_subset_pos >>
  quickCheck prop_subset_neg
  
