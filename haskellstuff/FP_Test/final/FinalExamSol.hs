{-# LANGUAGE TemplateHaskell #-}
module FinalExamSol where
  
import Test.QuickCheck
import Control.Monad
import Data.Char
  

-- Question 1

testQ1 :: ([String] -> String) -> Bool
testQ1 x = 
  x ["a","bb","ccc","dddd","eeeee","ffffff"] == "a" &&
  x ["uuuuuu","vvvvv","wwww","xxx","yy","z"] == "vvvvv" &&
  x ["uuuuuu","vvvva","vvvvv","xxx","yy","z"] == "vvvva" &&
  x ["Short","longer","???"] == "zzzzz"

-- 1a

ok :: String -> Bool
ok x = all isLower x && length x < 6

f :: [String] -> String
f xs = minimum ("zzzzz" : [x | x <- xs, ok x])

prop_1a = testQ1 f

-- 1b

g :: [String] -> String
g []                 = "zzzzz"
g (x:xs) | ok x      = x `min` g xs
         | otherwise = g xs

prop_1b = testQ1 g

-- 1c

h :: [String] -> String
h = foldr min "zzzzz" . filter ok

prop_1c = testQ1 h

prop_fgh xs = f xs == g xs && g xs == h xs

-- Question 2


-- 2a

i :: [a] -> [a] -> [a]
i xs ys = tail xs ++ [head ys]

prop_2a =
  i "abc" "def" == "bcd" &&
  i "def" "ghi" == "efg" &&
  i "ghi" "abc" == "hia"

testQ2 :: ([String] -> [String]) -> Bool
testQ2 x = 
  x ["abc","def","ghi"] == ["bcd","efg","hia"] &&
  x ["once","upon","a","time"] == ["nceu","pona","t","imeo"] &&
  x ["a","b","c"] == ["b","c","a"] &&
  x ["abc"] == ["bca"] &&
  x ["a"] == ["a"]

-- 2b

j :: [[a]] -> [[a]]
j xss = [ i xs ys | (xs,ys) <- zip xss (i xss xss) ]

prop_2b = testQ2 j

-- 2c

k :: [[a]] -> [[a]]
k (xs:xss) = l xs xss xs
  where
  l xs [] zs       = [i xs zs]
  l xs (ys:yss) zs = i xs ys : l ys yss zs

prop_2c = testQ2 k

nn :: [a] -> Bool
nn = not . null

prop_j :: [String] -> Property
prop_j xss =
  nn xss && all nn xss ==>
    i (concat xss) (concat xss) == concat (j xss)

prop_jk :: [String] -> Property
prop_jk xss =
  nn xss && all nn xss ==>
    j xss == k xss
    
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

-- 3a

eval :: Bool -> Bool -> Wff -> Bool
eval x y X          = x
eval x y Y          = y
eval x y Tr         = True
eval x y Fa         = False
eval x y (Not p)    = not (eval x y p)
eval x y (p :&: q)  = eval x y p && eval x y q
eval x y (p :|: q)  = eval x y p || eval x y q
eval x y (p :->: q) = eval x y p <= eval x y q

prop_3a
  =  eval False False ((X :->: Y) :&: (Not Y :|: X)) == True
  && eval False True  ((X :->: Y) :&: (Not Y :|: X)) == False
  && eval True  False ((X :->: Y) :&: (Not Y :|: X)) == False
  && eval True  True  ((X :->: Y) :&: (Not Y :|: X)) == True

-- 3b

simple :: Wff -> Bool
simple Tr = True
simple Fa = True
simple p  = check p
  where
  check :: Wff -> Bool
  check X          = True
  check Y          = True
  check Tr         = False
  check Fa         = False
  check (Not p)    = check p
  check (p :&: q)  = check p && check q
  check (p :|: q)  = check p && check q
  check (p :->: q) = check p && check q

prop_3b
  =  simple Tr                           == True
  && simple Fa                           == True
  && simple ((Tr :|: X) :->: (Tr :&: Y)) == False
  && simple ((X :|: Fa) :->: (Y :&: Fa)) == False
  && simple ((X :&: Y) :->: (X :|: Y))   == True

-- 3c

simplify :: Wff -> Wff
simplify X  = X
simplify Y  = Y
simplify Tr = Tr
simplify Fa = Fa
simplify (Not p) = snot (simplify p)
  where
  snot Tr = Fa
  snot Fa = Tr
  snot p  = Not p
simplify (p :&: q) = simplify p `sand` simplify q
  where
  Tr `sand` p = p
  Fa `sand` p = Fa
  p `sand` Tr = p
  p `sand` Fa = Fa
  p `sand` q  = p :&: q
simplify (p :|: q) = simplify p `sor` simplify q
  where
  Tr `sor` p = Tr
  Fa `sor` p = p
  p `sor` Tr = Tr
  p `sor` Fa = p
  p `sor` q  = p :|: q
simplify (p :->: q) = simplify p `simp` simplify q
  where
  Tr `simp` p = p
  Fa `simp` p = Tr
  p `simp` Tr = Tr
  p `simp` Fa = Not p
  p `simp` q  = p :->: q

prop_3c :: Bool
prop_3c
  =  simplify Tr                           == Tr
  && simplify Fa                           == Fa
  && simplify ((Tr :|: X) :->: (Tr :&: Y)) == Y
  && simplify ((X :|: Fa) :->: (X :&: Fa)) == Not X
  && simplify ((X :|: Y) :->: (X :&: Y))   == ((X :|: Y) :->: (X :&: Y))

prop_simplify :: Bool -> Bool -> Wff -> Bool
prop_simplify x y p  =
  simple (simplify p) &&
  simplify (simplify p) == simplify p &&
  eval x y p == eval x y (simplify p)

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
