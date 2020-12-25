-- Indexed data represented as a tree

{-# LANGUAGE TemplateHaskell #-}
module KeymapTreeSol ( Keymap,
                       size, depth,
                       get, set, del,
                       select,
                       toList, fromList,
                       merge, filterLT, filterGT,
                       prop_set_get, prop_toList_fromList,
                       testTree -- for autotest
                     )

where

-- Modules for testing
  
import Test.QuickCheck
import Control.Monad
import Data.List
  
-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving Eq

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right )

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v left right) = toList left ++ [(k,v)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = go
    where go Leaf = Nothing
          go (Node k v l r) | key == k = Just v
                            | key < k  = go l
                            | key > k  = go r

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,v):xs) = set k v (fromList xs)


-- Alternative higer-order solution:
--
-- fromList = foldr (uncurry set) Leaf

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where zs = zip (nub xs) ys


-- ** Optional Material

-- Exercise 12

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT x (Node k v l r) | k == x = r
                          | k < x  = filterGT x r
                          | k > x  = Node k v (filterGT x l) r

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT x (Node k v l r) | k == x = l
                          | k < x  = Node k v l (filterLT x r)
                          | k > x  = filterLT x l
                                     
-- Exercise 13
                                     
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf t = t
merge t Leaf = t
merge (Node k1 v1 l1 r1) x2@(Node k2 _ l2 r2)
  | k1 == k2  = Node k1 v1 (l1 `merge` l2) (r1 `merge` r2)
  | otherwise = Node k1 v1 (l1 `merge` filterLT k1 x2) (r1 `merge` filterGT k1 x2)
                
prop_merge :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge t1 t2 = sort (nubBy p (toList t1 ++ toList t2)) == toList (t1 `merge` t2)
  where p (k,_) (k',_) = k == k'

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del key = go
    where go Leaf = Leaf
          go (Node k v l r) | key == k = l `merge` r
                            | key < k  = Node k v (go l) r
                            | key > k  = Node k v l (go r)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f = go
  where go Leaf = Leaf
        go (Node k v l r) | f v       = Node k v (go l) (go r)
                          | otherwise = go l `merge` go r

-- Instances for QuickCheck -----------------------------

instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary

-- instance (Eq k, Eq a) => Eq (Keymap k a) where
--   (==) Leaf Leaf = True
--   (==) (Node k v leftNode rightNode) (Node k' v' leftNode' rightNode')
--    = k == k' && v == v' && leftNode == leftNode' && rightNode == rightNode'
--   (==) _ _ = False


-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
