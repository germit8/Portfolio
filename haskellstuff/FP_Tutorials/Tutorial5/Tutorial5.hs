module Tutorial5 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. Map
-- a.
doubles :: [Int] -> [Int]
doubles = map (*2)

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x -> fromIntegral x / 100)
-- c.
uppers :: String -> String
uppers = map (\x -> toUpper x)

-- d.
uppersComp :: String -> String
uppersComp xs = [ toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppersComp s


-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
above :: Int -> [Int] -> [Int]
above i = filter (\x -> x > i)

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x, y) -> x /= y)

-- d.
rmChar :: Char -> String -> String
rmChar c = filter (\x -> x /= c)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c xs = [x | x <- xs, x /= c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (\x -> x * 2) $ filter (\y -> y > 3) xs

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map (\x -> reverse x) $ filter (\y -> even $ length y) strs

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs


-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs 

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: Eq a => [[a]] -> Bool
prop_concat xs = concatFold xs == concatRec xs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec str1 [] = []
rmCharsRec [] str2 = str2
rmCharsRec (x:xs) str2 = rmCharsRec xs $ rmChar x str2

rmCharsFold :: String -> String -> String
rmCharsFold str1 str2 = foldr (rmChar) str2 str1

prop_rmChars :: String -> String -> Bool
prop_rmChars str1 str2 = rmCharsRec str1 str2 == rmCharsFold str1 str2


-- Matrix multiplication

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all (== head xs) xs

-- b.
valid :: Matrix -> Bool
valid [] = False
valid xs = all (\x -> length x > 0 && length x == length (head xs)) xs


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length $ head m

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM mat1 mat2 = 
    if (matrixHeight mat1 == matrixHeight mat2) && (matrixWidth mat1 == matrixWidth mat2) 
    then [ zipWith (+) x y | (x, y) <- (zip mat1 mat2)]
    else error "At least one of your matrices is not suitable for this operation"

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM mat1 mat2 =
    if matrixWidth mat1 == matrixHeight mat2
    then recSplit dot
    else error "At least one of your matrices is not suitable for this operation"
 where dot = map sum [ zipWith (*) x y | x <- mat1, y <- transpose mat2]
       
       recSplit [] = []
       recSplit xs = take (matrixWidth mat2) xs : (recSplit $ drop (matrixWidth mat2) xs) 
-- 8.
-- a. - 18

-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ x `f` y | (x, y) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) $ zip xs ys

-- ** Optional material

-- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
