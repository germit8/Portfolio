module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | x `mod` 2 == 0   = x `div` 2 : halveEvensRec xs
                     | otherwise        = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | x >= lo && x <= hi     = x : inRangeRec lo hi xs
                        | otherwise              = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0         = 1 + countPositivesRec xs
                         | otherwise     = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (x:xs) | isDigit x   = (digitToInt x) * multDigitsRec xs
                     | otherwise   = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = if not (elem ch [x | (x, y) <- xs]) then ch else [ y | (x, y) <- xs, x == ch] !! 0

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((x, y):xs) | x == ch    = y
                         | otherwise  = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)


-- 7.

normalize :: String -> String
normalize "" = ""
normalize (x:xs) | isAlpha x   = toUpper x : normalize xs
                 | isDigit x   = x : normalize xs
                 | otherwise   = normalize xs  

encipherStr :: Int -> String -> String
-- encipherStr k "" = ""
encipherStr k str = normString k (normalize str)
 where normString k "" = ""
       normString k (x:xs) | isDigit x    = x : (normString k xs)
                           | otherwise    = (encipher k (toUpper x)) : (normString k xs)


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (y, x) | (x, y) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x, y):xs) = (y, x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs


-- 9.

decipher :: Int -> Char -> Char
decipher i ch = lookUp ch (reverseKey (makeKey i))

decipherStr :: Int -> String -> String
decipherStr i "" = ""
decipherStr i str = normString i (normalize str)
 where normString i "" = ""
       normString i (x:xs) | isDigit x    = x : (normString i xs)
                           | otherwise    = (decipher i x) : (normString i xs)

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains str "" = True
contains "" substr = False
contains (x:xs) substr | x == substr !! 0 && isPrefixOf substr (x:xs) = True
                       | otherwise = contains xs substr


-- 11.

candidates :: String -> [(Int, String)]
candidates str = [(x, decipherStr x str) | x <- [0..25], (contains (decipherStr x str) "AND") || (contains (decipherStr x str) "THE")]


-- 12.

splitEachFive :: String -> [String]
splitEachFive "" = ["XXXXX"]
splitEachFive xs = [ if length x == 5 then x else x ++ (replicate (5 - (length xs `mod` 5)) 'X') | x <- cutString xs]
 where cutString "" = []
       cutString str = take 5 str : (cutString (drop 5 str))

prop_transpose :: String -> Bool
prop_transpose str = splitEachFive str == transpose (transpose (splitEachFive str))


-- 13.
encrypt :: Int -> String -> String
encrypt i str = concat (transpose (splitEachFive (encipherStr i str)))
-- (concat . transpose . splitEachFive) $ encipherStr i str
-- foldr (++) [] $ transpose $ splitEachFive $ encipherStr i str

-- 14.
decrypt :: Int -> String -> String
decrypt k str = concat decryptedList
 where decryptedList = transpose (reverseTranspose (decipherStr k str))

       reverseTranspose "" = []
       reverseTranspose xs = take ((length str) `div` 5) xs : (reverseTranspose (drop ((length str) `div` 5) xs))
