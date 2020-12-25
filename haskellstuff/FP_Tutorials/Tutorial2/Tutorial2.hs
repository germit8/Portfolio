module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, x `mod` 2 == 0]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo && x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

-- b) I think it is because list comprehensions only generate new lists based on some possible guards. We can kind of "loop" through it using recursion, but then to count the
--    positive numbers, we would need some variable like num_of_positives and change it on iterations.
--    Here the list comprehension generates the desired list of only positive numbers, but then we still need to apply length function to it

-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [ digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length [ x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9^(countDigits xs)


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : [ toLower y | y <- xs]



-- 6. title

-- lowercase :: String -> String
-- lowercase xs = [ toLower x | x <- xs]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [ if length y > 3 then capitalise y else lowercase y | y <- xs]
 where lowercase string = [ toLower w | w <- string]


-- 7. signs

sign :: Int -> Char
sign i | i < (-9) || i > 9 = error "Unacceptable input"
       | i == 0 = '0' 
       | i >= 1 && i <= 9 = '+' 
       | otherwise = '-'

signs :: [Int] -> String
signs xs = [ if y >= 0 then if y >= 1 && y <= 9 then '+' else '0' else '-' | y <- xs, (y <= 9) && (y >= (-9))]


-- 8. score
-- isVowel :: Char -> Bool
-- isVowel x = elem x "aeiou"

score :: Char -> Int
score x | isLower x && isVowel x || isUpper x && not (isVowel (toLower x)) = 2
        | not (isAlpha x) = 0
        | isUpper x && isVowel (toLower x) = 3 
        | otherwise = 1
 where isVowel x = elem x "aeiou"

-- isUpper x && not (isVowel x) || isLower x && isVowel x = 2

totalScore :: String -> Int
totalScore xs = product [ score x | x <- xs, isAlpha x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- 9. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [ round (discount x) | x <- prices, (discount x) < 19900]
 where discount y = (fromIntegral y) * 0.9

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [ x | x <- xs, x > 0]

-- Optional Material

-- 10. crosswordFind

-- sometimes it doesn't work on codegrade when quickcheck uses the backslash + numbers as a letter...
-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = 
    if and [ pos >= 0 && pos < length y && length y > 0 | y <- words]
        then [ x | x <- words, x !! pos == letter && length x == len] 
    else []


-- 11. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [ y | (x, y) <- (zip str [0..]), x == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str


-- 12. contains

contains :: String -> String -> Bool
contains str substr = or [ take (length substr) (drop x str) == substr | x <- [0..length str], isPrefixOf substr (drop x str)]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 | (contains str1 str2) || (contains str1 str2 == False) && length str2 <= length str1 = True
                        | ((length str1 < length str2) || (length str1 == length str2)) && (contains str1 str2 == False) = True
                        | otherwise = False