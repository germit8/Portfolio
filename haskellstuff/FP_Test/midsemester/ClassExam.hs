-- Informatics 1 - Functional Programming 
-- Class Test 2020

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Bool
f xs = and [ isUpper x | x <- xs, isAlpha x]
-- First I check if character isAlpha - if not, it ignores it -> and[] gives True.
-- The list then comprises only of True or False based on if alphabetic characters are either upper or lower
-- All of these characters must be upper -> everything in the list must be True

-- b

g :: String -> Bool
g "" = True
g (x:xs) | isAlpha x = isUpper x && g xs
         | otherwise = g xs
-- It checks if isAlpha x, then if isUpper x - if it is, it just calls the recursive function again
-- If a single alphabetic is not upper, then the whole thing is False
-- It keeps on calling the function until the list is empty - that means it has not found a single lower character -> it is True
-- "Otherwise" ignores all special characters and just calls the recursive function again

-- c

prop_fg :: String -> Bool
prop_fg xs = g xs == f xs

-- Problem 2

-- a

c :: String -> Bool
c xs = if length xs >= 2 
       then or [xs !! (index - 1) == xs !! index | index <- [1..(length xs - 1)]]
       else False
-- First it checks if length of string is >= 2, becuase there has to be at least 2 chars in str
-- Then we compare if index x - 1 and index x are the same - we take x from list which starts at 1 (x - 1 = 0)
-- and ends at length of string - 1 -> string of length 8 has indexes from 0..7
-- I use (x - 1) becuase this way we can't get the error that index is too large - last indices we compare are 6 and 7

-- b

d :: String -> Bool
d "" = False
d [x] = False
d (x:y:xs) | x == y = True
           | otherwise = d (y:xs)
-- if string is empty or consists only of one element, then it is automatically false
-- then it recursively splits on first two elements of the string, which are then compared
-- if they are equal, then the function returns true, otherwise it calls the function without the first element of string

-- c

prop_cd :: String -> Bool
prop_cd xs = c xs == d xs
