-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial9Sol where
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
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

-- 2.
intersperse :: a -> [a] -> [a]
intersperse sep []     = [sep]
intersperse sep (y:ys) = sep : y : intersperse sep ys

-- 3.
showRow :: String -> String
showRow = concat . intersperse "|" . group

-- 4. instead of showMat
showGrid :: Matrix Digit -> [String]
showGrid = showCol . map showRow
  where
    showCol = concat . intersperse [bar] . group
    bar     = replicate 13 '-'

-- 5.
put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

-- 6.
showMat :: Matrix Digit -> String
showMat = concatMap (map dot)
  where
  dot ' ' = '.'
  dot d   = d

readMat :: String -> Matrix Digit
readMat = map (map undot) . groupBy 9
  where
  undot '.' = ' '
  undot d   = d

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices = map (map choice)
  where choice d | d `elem` digits = [d]
                 | blank d         = digits

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand = cp . map cp

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (map length (concat m))

-- 10.
easySols :: Integer
easySols = product $ map (fromIntegral . length) (concat $ choices easy)

-- 11, 12, 13
rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- ** commented these out because they need a proper Sudoku generator
-- prop_rows, prop_cols, prop_boxs :: Matrix Digit -> Bool
-- prop_rows m = rows (rows m) == m
-- prop_cols m = cols (cols m) == m
-- prop_boxs m = boxs (boxs m) == m

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = all distinct (rows g)
           && all distinct (cols g)
           && all distinct (boxs g)
      
-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

-- 17.
splits :: [a] -> [(a, [a])]
splits []     = []
splits (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- splits xs ]

single :: [Digit] -> Bool
single [d] = True
single _   = False

the :: [Digit] -> Digit
the [d] = d

fixed :: Row [Digit] -> [Digit]
fixed row = [ the ds | ds <- row, single ds ]

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [ ds \\ fixed rest | (ds,rest) <- splits row ]

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 19.
many :: Eq a => (a -> a) -> a -> a
many f x | x == y    = x
         | otherwise = many f y
         where y = f x

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract mat | all (all single) mat = map (map the) mat

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve = extract . many prune . choices


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed = any (any null)

-- 23.
solved :: Matrix [Digit] -> Bool
solved = all (all single)

-- 24 `.
shortest :: Matrix [Digit] -> Int
shortest = minimum . filter (> 1) . map length . concat

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat =
  [ preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds ]
  where
    short ds              = length ds == shortest mat
    (preMat, row:postMat) = break (any short) mat
    (preRow, ds:postRow)  = break short row

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search = loop . choices
  where
    loop :: Matrix [Digit] -> [Matrix Digit]
    loop mat | solved pruned = [extract pruned]
             | failed pruned = []
             | otherwise     = [ sol | exp <- expand1 pruned, sol <- loop exp ]
             where
               pruned = many prune mat

-- Computing the search tree
-- (How hard was it to solve the puzzle?)

data Tree = Fail Int
          | Soln Int
          | Node Int [Tree]
  deriving (Show)

howmany :: Eq a => (a -> a) -> a -> (Int, a)
howmany f x | x == y    = (0, x)
            | otherwise = (n+1, z)
            where  y = f x
                   (n,z) = howmany f y 

searchTree :: Matrix Digit -> Tree
searchTree = loop . choices
  where
  loop mat | solved pruned = Soln n
           | failed pruned = Fail n
           | otherwise     = Node n (map loop (expand1 pruned))
           where (n, pruned) = howmany prune mat

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

size :: Matrix [Digit] -> Integer
size = product . map genericLength . concat

g1 :: Matrix Digit
g1 = replicate 9 digits

                 
-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

mainQC :: IO ()
mainQC = do  
  qc <- $forAllProperties $ quickCheckWithResult $ stdArgs {maxSize = 5}
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
