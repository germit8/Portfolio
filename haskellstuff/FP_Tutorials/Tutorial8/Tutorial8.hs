module Tutorial8 where

import System.Random

-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]
--longestProductLen [("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),("0903900739533", ("Bagpipes of Glory", "6-CD Box")),("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),("0042400212509", ("Universal deep-frying pan", "pc"))]

-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [length $ fst y | (x, y) <- xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine l tpl = fst tpl ++ "..." ++ fst (snd tpl) ++ replicate (3 + l - length (fst $ snd tpl)) '.' ++ snd (snd tpl)

showCatalogue :: Catalogue -> String
showCatalogue cata = unlines [formatLine (longestProductLen $ toList cata) x | x <- toList cata] -- instead of unlines can be used ++ "\n"
-- [(Barcode, (Product, Unit))]
 
     
-- Exercise 2
-- get "9780201342758" testDB -> Just ("Thompson - \"Haskell: The Craft of Functional Programming\"","Book")
-- get "000" testDB -> Nothing
-- a) it returns type Maybe a, which is represented either as Just a or Nothing
-- Values are: Int, String, List, Tuple, Float
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | (Just x) <- xs]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs cata = catMaybes [get x cata | x <- xs] -- [Maybe a]

-- Exercise 4
{-
a)
*Tutorial8> theDB <- readDB
Done
(0.80 secs, 873,319,944 bytes)

b)
I took first, middle and last key from keys, because I don't know how exactly samples chooses the keys
and if it takes them randomly "in order" of the database, there is a chance that the first key is also at the start
of the database, middle key is somewhere in the middle and the last one is somewhere near the end

-- first key from keys (100 keys)
*Tutorial8> get "0075596166126" theDB
Just ("\"ROCK","CD")
(0.02 secs, 71,192 bytes)

-- approx middle key from keys
*Tutorial8> get "0829707900198" theDB
Just ("T.V. Eye Video Magazine 1 & 2","DVD")
(0.02 secs, 88,040 bytes)

-- last key from keys
*Tutorial8> get "0017854109056" theDB
Just ("*MICHELINAS ITAL GARDN BSTRO","9 oz")
(0.02 secs, 88,040 bytes)

getItems keys theDB = (3.69 secs, 181,783,304 bytes) = average 2.59 secs / 100 = 0.0259 which is approx 0.02 secs per search
                      (2.21 secs, 3,007,968 bytes)
                      (2.23 secs, 3,007,968 bytes)
                      (2.22 secs, 3,007,968 bytes)
The first search for items seems to always be slower than the consecutive ones, I suppose because it is because of caching?


c) It doesn't have to interact with the item in any way as long as the barcode does not match,
   so it just takes the item when the barcode matches
   I think the time it takes is O(n) and if we doubled the size of the database,
   it would be O(2n), which is still O(n), because we do not care about constants
-}
-- Exercise 5
-- I expected 1 but got an error, because Data constructor Leaf is not in scope
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 11
{-
a)
*Tutorial8> theDB <- readDB
Done
(5.30 secs, 2,017,474,544 bytes)

b)
*Tutorial8> get "0013700046308" theDB
Just ("Trash Bags-Hefty-Ultra Flex","30 gal. 30 ct.")
(0.01 secs, 103,360 bytes)

*Tutorial8> get "0013700897481" theDB
Just ("HEFTY 1 ZIP PATRIOTIC SHPR 10.00 CT","48 ct")
(0.01 secs, 99,784 bytes)

*Tutorial8> get "0050086007950" theDB
Just ("PAPERBACK BOOKS","EACH")
(0.01 secs, 86,808 bytes)

getItems keys theDB = (0.55 secs, 3,698,800 bytes), 0.55 / 100 = 0.0055 sec on average, which I believe is 0.01 for a single item, it is just rounded up
                      (0.54 secs, 3,498,528 bytes)
                      (0.56 secs, 3,498,528 bytes)

c) At most it should inspect O(n) barcodes. The tree is ordered, but it doesn't necessarily have to be balanced, which means that the tree can be just a super long branch
   In the best case, it would inspect just O(log n) - if all the branches were at least very similar in length

d) It takes much longer to load the database when using the tree keymap, but once it is loaded, it is so much faster to go through the tree database than through list
-}

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)

samples :: Int -> Catalogue -> IO [Barcode]
samples n db = sequence [getSample db | i <- [1..n]]

gets :: [Barcode] -> Catalogue -> [Item]
gets ks db  =  [x | k <- ks, Just x <- [get k db]]
