--import Test.QuickCheck

squares :: [Int] -> [Int]
squares xs = [ x^2 | x <- xs, x `mod` 2 == 0]

square :: Integer -> Integer
square x = x * x

angleVectors :: Float -> Float -> Float -> Float -> Float
angleVectors a b a' b' = acos phi where phi = (dotProduct a b a' b') / (lengthVector a b * lengthVector a' b')

dotProduct :: Float -> Float -> Float -> Float -> Float
dotProduct x y x' y' = (x * x') + (y * y')

lengthVector :: Float -> Float -> Float
lengthVector m n = sqrt (dotProduct m n m n)

root :: Float -> Float -> Float -> Float
root a b c =((-b) + (discr a b c)) / (2 * a)

discr :: Float -> Float -> Float -> Float
discr x y z = sqrt (y * y - 4 * x * z)

hour :: Int -> Int
hour h = if 1 + minToHour h <= 12 then 1 + minToHour h else 1 + minToHour h

minToHour :: Int -> Int
minToHour x = div x 60

between :: Int -> Int -> Int -> Int
between a b c | (a <= b || a <= c) && (a > c || a > b) = a | (b < a || b < c) && (b > c || b > a) = b | otherwise = c

xor :: Bool -> Bool -> Bool
xor x y = if ((x == True && y == False) || (x == False && y == True)) then True else False

squaresRec :: [Int] -> [Int]
squaresRec [] = []
squaresRec (x : xs) = x * x : squaresRec xs

squareCond :: [Int] -> [Int]
squareCond ws = if null ws then []
else
    let
        x = head ws
        xs = tail ws
    in
        x * x : squareCond xs

oddsRec :: [Int] -> [Int]
oddsRec [] = []
oddsRec (x : xs) | odd x = x : oddsRec xs | otherwise = oddsRec xs

sumRec :: [Int] -> Int
sumRec [] = 0
sumRec (x : xs) = x + sumRec xs

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x = the [ y | (w,y) <- xys, x == w ]
 where
 the [x] = x

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []
 where
     insert :: Ord a => a -> [a] -> [a]
     insert x []                 = [x]
     insert x (y:ys) | x <= y    = x:y:ys
                     | otherwise = y : insert x ys

-- Quiksort
qsort :: Ord a => Int -> [a] -> [a]
qsort k xs | length xs <= k = isort xs
qsort k (y:xs) = qsort k [x | x <- xs, x < y] ++ [y] ++ qsort k [x | x <- xs, x >= y]

-- Merge sort
msort :: Ord a => Int -> [a] -> [a]
msort k xs | length xs <= k   = isort xs
           | otherwise        = merge (msort k (take m xs)) (msort k (drop m xs))
 where
     m = length xs `div` 2

     merge :: Ord a => [a] -> [a] -> [a]
     merge xs []          = xs
     merge [] ys          = ys
     merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                         | otherwise = y : merge (x:xs) ys

choose :: Int -> [a] -> [[a]]
choose 0 [] = [[]]
choose k (x:xs) | k == 0 = [[]]
                | k == n = [x:xs]
                | 0 < k && k < n = choose k xs ++ map (x:) (choose (k-1) xs)
 where
    n = length (x:xs)

partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions n | n > 0 = [ k : xs | k <- [1..n], xs <- partitions (n-k), all (k <=) xs ]
