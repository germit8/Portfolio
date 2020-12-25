module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy 0 _ = [[]]
groupBy _ [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)


-- 2.
intersperse :: a -> [a] -> [a]
intersperse y = foldr (\x -> ([y, x] ++)) [y]

-- 3.
showRow :: String -> String
showRow str = concat $ intersperse "|" (group str)

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid xs = concat $ intersperse [replicate 13 '-'] (group [showRow x | x <- xs])
-- why are we not using 'intercalate' ?

-- 5.
put :: Matrix Digit -> IO ()
put xs = (putStr . unlines) (showGrid xs)

-- 6.
showMat :: Matrix Digit -> String
showMat = concatMap (map (\x -> if x == ' ' then '.' else x))

readMat :: String -> Matrix Digit
readMat str = groupBy 9 (map (\x -> if x == '.' then ' ' else x) str)

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices [] = []
choices (xs:xss) = (map (\x -> if x == " " then "123456789" else x) (groupBy 1 xs)) : choices xss

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`


--expand [["12","34"], ["56","78"]]
expand :: Matrix [Digit] -> [Matrix Digit]
expand [] = []
expand [[]] = []
expand matrix = cp (expand' matrix)
 where expand' [] = []
       expand' (xs:xss) = cp xs : expand' xss

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == 2^sum [length x | x <- m]

-- 10.
easySols :: Integer
easySols = product (map (foldr (\xs -> (*) (fromIntegral (length xs))) 1) (choices easy))
-- *Tutorial9> easySols
-- 78551672112789411833022577315290546060373041 (== 9^46)
-- in easy sudoku there are 35 given spots out of 81, so 46 are missing. On every empty spot, there can be 9 different digits
-- so there is 9^46 different combinations
-- It would take approximately 2.5*10^24 years for the computer to calculate them
-- which is approximately 1.8*10^14 times more than how old the universe is.

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows xs = xs
cols = transpose
boxs xs = (map ungroup . ungroup) (map cols ((group . map group) xs))
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = xs == nub xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = and [ distinct x | x <- g] && and [ distinct x | x <- cols g] && and [ distinct x | x <- boxs g]

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices
-- this is definitely not a valid way

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (delete singletons) row
 where singletons = [the x | x <- row, length x == 1]

       delete digs [x] = [x]
       delete digs xs = filter (`notElem` digs) xs

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune matrix = pruneBy rows (pruneBy cols (pruneBy boxs matrix))

-- 19.
close :: (Eq a, Ord a) => [(a,a)] -> [(a,a)]
close pairs = nub (sort (pairs ++ [ (x,z) | (x,y) <- pairs, (y',z) <- pairs, y == y' ]))

many :: Eq a => (a -> a) -> a -> a
many f x = if f x /= x then many f (f x) else x

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract [] = []
extract (x:xs) = [the y | y <- x] : extract xs

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve matrix = extract $ many prune $ choices matrix

-- we can solve all except hard and evil with pruning

exmp :: Matrix [Digit]
exmp = [["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        ]

-- extract exmp == ["123456789","123456789","123456789","123456789","123456789","123456789","123456789","123456789","123456789"]


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed [] = False
failed (x:xs) = or [length y == 0 | y <- x] || failed xs

-- 23.
solved :: Matrix [Digit] -> Bool
solved [] = True
solved (x:xs) = and [length y == 1 | y <- x] && solved xs

-- 24.
shortest :: Matrix [Digit] -> Int
shortest mat = minimum (filter (/= 1) (lengthList mat))
 where lengthList xs = concatMap (map length) xs

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [fst matBreak ++ [fst rowBreak ++ [[d]] ++ tail (snd rowBreak)] ++ tail (snd matBreak) | d <- head (snd rowBreak)]
 where
       matBreak = break (\row -> shortest mat `elem` [length x | x <- row]) mat -- == (preMat, row:postMat) == (fst, (head snd):(tail snd))
       rowBreak = break (\ds -> length ds == shortest mat) (head (snd matBreak)) -- == (preRow, ds:postRow)


exmpl1 :: Matrix [Digit]
exmpl1 = [["9","1578","3","67","167","4","2","15678","5678"],
          ["4","178","6","5","12379","127","138","13789","789"],
          ["57","157","2","8","13679","167","135","1345679","5679"],
          ["238","238","9","267","2678","5","138","13678","4"],
          ["58","6","7","1","4","3","9","2","58"],
          ["1","23458","45","9","2678","267","358","35678","5678"],
          ["2356","123459","145","246","1256","8","7","59","259"],
          ["2567","1257","15","267","12567","9","4","58","3"],
          ["257","24579","8","3","257","27","6","59","1"]]

-- 26.
{-
1. Every list of digits contains only one element. Then we can use extract to get the solution.
2. Some list of digits is empty. Then, there is no possible solution.
3. Some list of digits contains more than one element.

Using failed, solved, extract, many, prune, and expand1, write a search program
-}
search :: Matrix Digit -> [Matrix Digit]
search matrix = [extract x | x <- partialSols (expand1 (choices matrix)), valid (extract x)]

-- expand1 matrix
partialSols :: [Matrix [Digit]] -> [Matrix [Digit]]
partialSols [] = []
partialSols (xs:xss) | solved xs = many prune xs : partialSols xss
                     | failed xs = partialSols xss
                     | otherwise = partialSols [many prune x | x <- expand1 xs] ++ partialSols xss

-- book has 1 solution
-- easy has 1 solution
-- medium has 1 solution
-- hard has 1 solution
-- evil has 17 solutions

-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

