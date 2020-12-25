-- Indexed data represented as a tree

module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
                    prop_set_get, prop_toList_fromList
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf))

testTree2 :: Keymap Int Int
testTree2 = Node 3 30 (Node 2 20 Leaf Leaf)
                     (Node 4 40 Leaf 
                               (Node 5 50 Leaf (Node 6 60 Leaf Leaf)))

-- Exercise 6
{-
Node k a (Keymap k a) (Keymap k a)
It does not care about the values of 'k' and 'a', it only matters that they exist, which means that it is not empty and so we add +1
then we recursively call size on left and right branch, because those branches are both "(Keymap k a)". The recursion ends when a branch contains only a Leaf
and provides no fruit in form of Nodes, so we just add 0
-}
size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) | depth left >= depth right  = 1 + depth left
                            | otherwise                  = 1 + depth right

-- depth (Node _ _ left right) = 1 + max (depth left) (depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node x y left right) = toList left ++ [(x, y)] ++ toList right
-- to get it sorted, we first recursively create the left branch, because everything to the left is smaller than to the right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)
-- if it encounters leaf, it just return a Node with the desired key and value
-- this function creates and inserts a new node in its correct spot (by correct I mean ordered by key), if it is not already in the list
-- as long as it is encountering nodes, it compares the key with them. If the keys match, then it does nothing, just returns original keymap
-- if it does not, it goes deeper until it encounters a leaf, which means that this element is not in the keymap, so it adds it on the place of leaf

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key Leaf = Nothing
get key (Node k a left right) | key == k = Just a
                              | key < k  = get key left
                              | key > k  = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- set :: Ord k => k -> a -> Keymap k a -> Keymap k a
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf -- recursively it would be set (fst x) (snd x) (fromList xs) and empty list would give Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key Leaf = Leaf
filterLT key (Node k v left right) | key > k   = Node k v left (filterLT key right)
                                   | otherwise = filterLT key left

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key Leaf = Leaf
filterGT key (Node k v left right) | key < k   = Node k v (filterGT key left) right
                                   | otherwise = filterGT key right

-- Exercise 13

mergeList :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
mergeList kmp1 kmp2 = fromList (toList kmp1 ++ toList kmp2)

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge x Leaf = x
merge Leaf y = y
merge (Node k v left right) y = Node k v (merge left (filterLT k y)) (merge right (filterGT k y))


prop_merge :: Ord k => Keymap k a -> Keymap k a -> Bool
prop_merge kmp1 kmp2 = size (merge kmp1 kmp2) <= size kmp1 + size kmp2

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf
del key (Node k v left right) | key == k  = merge left right
                              | key < k   = Node k v (del key left) right
                              | key > k   = Node k v left (del key right)

-- del with list comprehension
del2 :: Ord k => k -> Keymap k a -> Keymap k a
del2 key kmp = fromList [(x, y) | (x, y) <- toList kmp, x /= key]

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select pred Leaf = Leaf
select pred (Node k v left right) | pred v    = Node k v (select pred left) (select pred right)
                                  | otherwise = merge (select pred left) (select pred right)

-- select with list comprehension
select2 :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select2 pred kmp = fromList (filter (\(x, y) -> pred y) (toList kmp))

-- select (\x -> x `mod` 20 == 0) testTree == [(2,20),(4,40)]
-- select (\x -> x `mod` 20 == 0) testTree2 == [(2,20),(4,40),(6,60)]

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
