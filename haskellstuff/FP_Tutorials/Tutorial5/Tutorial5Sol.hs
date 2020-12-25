-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial5Sol where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = map double xs
    where double x = x * 2

-- using partial application:
doubles' :: [Int] -> [Int]
doubles' = map (* 2)

-- b.
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map f xs
    where f x = fromIntegral x / 100

-- c.
uppers :: String -> String
uppers str = map toUpper str

-- d.
uppersComp :: String -> String
uppersComp str = [toUpper c | c <- str]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppersComp s

-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- using partial application:
alphas' :: String -> String
alphas' = filter isAlpha

-- b.
above :: Int -> [Int] -> [Int]
above limit xs = filter aboveLimit xs
    where aboveLimit x = limit < x

-- using partial application:
above' :: Int -> [Int] -> [Int]
above' limit = filter (limit <)

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xys = filter unequal xys
    where unequal (x,y) = x /= y

-- using partial application:
unequals' :: [(Int,Int)] -> [(Int,Int)]
unequals' = filter (uncurry (/=))

-- d.
rmChar :: Char -> String -> String
rmChar ch str = filter notch str
  where notch c = c /= ch

-- using partial application:
rmChar' :: Char -> String -> String
rmChar' ch = filter (/= ch)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp ch str = [c | c <- str, c /= ch]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map double (filter greaterThan3 xs)
    where greaterThan3 x = x > 3
          double x = x * 2

-- using partial application and composition:
largeDoubles'' :: [Int] -> [Int]
largeDoubles'' = map (*2) . filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter evenLength strs)
    where evenLength s = even (length s)

-- using partial application and composition:
reverseEven'' :: [String] -> [String]
reverseEven'' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs


-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec []       = []
concatRec (xs:xss) = xs ++ concatRec xss

concatFold :: [[a]] -> [a]
concatFold xss = foldr (++) [] xss

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec []     str = str
rmCharsRec (c:cs) str = rmChar c (rmCharsRec cs str)

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


-- Matrix multiplication

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform []     = True
uniform (x:xs) = all (== x) xs

-- The library function 'all' can be defined as:
--
-- all :: (a -> Bool) -> [a] -> Bool
-- all p xs = foldr (&&) True (map p xs)
--
-- Or using partial application and composition:
--
-- all p = foldr (&&) True . map p

-- b.
valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

-- using partial application and composition:
valid' :: Matrix -> Bool
valid' m = uniform (map length m)             -- (1)
        && not (null m) && all (not . null) m -- (2)


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height m = length m

-- plusRow :: [Rational] -> [Rational] -> [Rational]
-- plusRow = zipWith (+)

plusM :: Matrix -> Matrix -> Matrix
plusM m n | ok        = zipWith (zipWith (+)) m n
          | otherwise = error "Invalid input matrices"
  where ok = valid m && valid n
             && width m == width n
             && height m == height n


-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | ok        = [ [ dot row col | col <- transpose m2 ] | row <- m1 ]
             | otherwise = error "Invalid input matrices"
  where dot xs ys = sum (zipWith (*) xs ys)
        ok        = valid m1 && valid m2
                    && width m1 == height m2


-- 8
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f x y | (x,y) <- zip xs ys ]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)


-- ** Optional material

-- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map (map f)

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = zipWith (zipWith f)

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes []     = []
removes (x:xs) = xs : map (x :) (removes xs)

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = map (map transpose . removes . transpose) (removes m)

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = cycleN h [evenRow, oddRow]
  where evenRow     = cycleN w [1,-1]
        oddRow      = cycleN w [-1,1]
        cycleN n xs = take n (cycle xs)
        
determinant :: Matrix -> Rational
determinant [[x]] = x
determinant m = sum (zipWith (*) row (cycle [1,-1]))
  where f x m = x * determinant m
        row   = head (zipMatrix f m (minors m))

cofactors :: Matrix -> Matrix
cofactors m = zipMatrix (*) (mapMatrix determinant (minors m)) signs
  where signs = signMatrix (width m) (height m)
        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = mapMatrix (k *)

inverse :: Matrix -> Matrix
inverse [[r]] = [[denominator r % numerator r]] -- spacial case for 1x1 matrix
inverse m = scaleMatrix (1 / determinant m) (transpose (cofactors m))

-- Tests
identity :: Int -> Matrix
identity n = map f [0..n - 1]
  where f m = take n (replicate m 0 ++ [1] ++ repeat 0)

prop_inverse1 :: Rational -> Property
prop_inverse1 a = determinant m /= 0 ==> 
                       m `timesM` inverse m    == identity 1
                       && inverse m `timesM` m == identity 1
  where m = [[a]]

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = determinant m /= 0 ==> 
                       m `timesM` inverse m    == identity 2
                       && inverse m `timesM` m == identity 2
  where m = [[a,b],[c,d]]

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = determinant m /= 0 ==> 
                         m `timesM` inverse m    == identity 3
                         && inverse m `timesM` m == identity 3
  where m           = [row r1, row r2, row r3]
        row (a,b,c) = [a,b,c] 


-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do  
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
