module Tutorial6 where

import Data.List (nub, delete, sortOn)
--import Data.Sort(uniqueSort, sort)
import Data.Maybe  (mapMaybe)
import Control.Monad( liftM, liftM2 )
import Debug.Trace
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )


-- ** Warmup exercises

-- The datatype 'Fruit'

data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Apple _ _) = False
isBloodOrange (Orange name _) = name `elem` ["Sanguinello", "Moro", "Taroco"]

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fruitList = sum [ num | (Orange name num) <- fruitList, isBloodOrange (Orange name num)]
-- type constructors -> pattern mathcing

-- 3.
worms :: [Fruit] -> Int
worms fruitList = length [ 1 | (Apple name hasWorm) <- fruitList, hasWorm]


-- ** Implementing propositional logic in Haskell

-- The datatype 'Wff' a
data Wff a = V a
           | T
           | F
           | Not (Wff a)
           | Wff a :|: Wff a
           | Wff a :&: Wff a
           | Wff a :->: Wff a
           | Wff a :<->: Wff a
           deriving (Eq, Ord)
infixr 3 :&:
infixr 2 :|:
infixr 1 :->:
infixr 0 :<->:

 
data Atom = A|B|C|D|P|Q|R|S|W|X|Y|Z deriving (Eq, Show, Ord)
-- we will use these as propositional letters in examples         
type Env a = [(a, Bool)]

lookUp :: Eq a => Env a -> a -> Bool
lookUp v x = the [ b | (x', b) <- v, x == x' ]
    where the [z] = z
          the []  = error ("valuation undefined")
          the zs  = error ("multiple values")
-- we represent valuations abstractly as functions.
-- The code above generates such a funcion from an association list.


-- Functions for handling Wffs
-- Use a function to substitute values for atoms
substitute :: (a -> b) -> Wff a -> Wff b
substitute _ T           = T
substitute _ F           = F
substitute f (Not p)     = Not (substitute f p)
substitute f (p :|: q)   = substitute f p :|: substitute f q
substitute f (p :&: q)   = substitute f p :&: substitute f q
substitute f (V a)       = V (f a)
substitute f (p :->: q)  = substitute f p :->: substitute f q
substitute f (p :<->: q) = substitute f p :<->: substitute f q

-- evaluate a Wff whose atoms are Booleans
evaluate :: Wff Bool -> Bool 
evaluate T           = True
evaluate F           = False
evaluate (Not p)     = not (evaluate p)
evaluate (p :&: q)   = evaluate p && evaluate q
evaluate (p :|: q)   = evaluate p || evaluate q
evaluate (V b)       = b
evaluate (p :->: q)  = not (evaluate p) || evaluate q
evaluate (p :<->: q) = evaluate p == evaluate q

-- evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval v wff = evaluate ( substitute (lookUp v) wff )

-- list the atoms that occur in a wff - 
--  NOTE: atoms in the result must be unique
atoms :: Eq a => Wff a -> [a]
atoms (V x)       = [x]
atoms (F)         = []
atoms (T)         = []
atoms (Not p)     = atoms p
atoms (p :|: q)   = nub (atoms p ++ atoms q)
atoms (p :&: q)   = nub (atoms p ++ atoms q)
atoms (p :->: q)  = nub (atoms p ++ atoms q)
atoms (p :<->: q) = nub (atoms p ++ atoms q)

-- creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []     = [[]]
envs (x:xs) = [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Eq a => Wff a -> Bool
satisfiable p = or [ eval e p | e <- envs (atoms p)]

models :: Eq a => Wff a -> Int
models p = length [ e | e <- envs (atoms p),  eval e p ]

wffTry :: Wff Atom
wffTry = ((V A :|: V B) :&: (Not (V B) :|: Not (V C)) :&: (Not (V C) :|: V D) :&: (Not (V A) :|: Not (V P)) :&: (Not (V P) :|: V D))


-- ** Exercises

-- 4.
wff1 :: Wff Atom
wff1 = (V P :|: V Q) :&: (V P :&: V Q)
-- ((P ∨ Q) ∧ (P ∧ Q))
-- satisfiable wff1 == True
{- table wff1 ==
P Q | (P | Q) & P & Q
- - | ---------------
F F |        F       
F T |        F
T F |        F       
T T |        T
-}

wff2 :: Wff Atom
wff2 = ((V P :&: (V Q :|: V R)) :&: ((Not (V P) :|: Not (V Q)) :&: (Not (V P) :|: Not (V R))))
-- ((P ∧ (Q ∨ R)) ∧ (((¬P) ∨ (¬Q)) ∧ ((¬P) ∨ (¬R))))
-- satisfiable wff2 == False
{- table wff2
P Q R | P & (Q | R) & (~P | ~Q) & (~P | ~R)
- - - | -----------------------------------
F F F |                  F
F F T |                  F
F T F |                  F
F T T |                  F
T F F |                  F
T F T |                  F
T T F |                  F
T T T |                  F
-}


-- 5. 
tautology :: Eq a => Wff a -> Bool
tautology formula = and [eval x formula | x <- (envs $ atoms formula)]
-- wff1 == False, wff2 == False, Not (wff1) == False, Not (wff2) == True

-- i.  either P is a tautology, or ~P is satisfiable
prop_taut1 :: Wff Atom -> Bool
prop_taut1 p = tautology p || satisfiable (Not (p))

-- ii.  either P is not satisfiable, or ~P is not a tautology
prop_taut2 :: Wff Atom -> Bool
prop_taut2 p = not (satisfiable p) || not (tautology $ Not (p))

-- Any other properties you can think of?
prop_taut :: Wff Atom -> Bool
prop_taut p  = undefined

-- 6.
wff3 :: Wff Atom
wff3 = ((V P :->: V Q) :&: (V P :&: Not (V Q)))
-- ((P → Q) ∧ (P ∧ (¬Q)))
-- satisfiable wff3 == False
{- table wff3
P Q | (P -> Q) & P & ~Q
- - | -----------------
F F |         F        
F T |         F
T F |         F
T T |         F
-}

wff4 :: Wff Atom
wff4 = ((V P :<->: V Q) :&: ((V P :&: Not (V Q)) :|: (Not (V P) :&: V Q)))
-- ((P ↔ Q) ∧ ((P ∧ (¬Q)) ∨ ((¬P) ∧ Q)))
-- satisfiable wff4 == False
{- table wff4
P Q | (P <-> Q) & (P & ~Q | ~P & Q)
- - | -----------------------------
F F |               F
F T |               F
T F |               F
T T |               F  
-}

-- 7.
equivalent :: Eq a =>  Wff a -> Wff a -> Bool
equivalent p q = and [eval x p == eval x q | x <- envs $ nub $ atoms p ++ atoms q]

-- 8.
subformulas :: Eq a => Wff a -> [Wff a]
subformulas (V x)       = [V x]
subformulas (T)         = [T]
subformulas (F)         = [F]
subformulas (Not p)     = (Not p) : subformulas p
subformulas (p :|: q)   = (p :|: q) : nub (subformulas p ++ subformulas q)
subformulas (p :&: q)   = (p :&: q) : nub (subformulas p ++ subformulas q)
subformulas (p :->: q)  = (p :->: q) : nub (subformulas p ++ subformulas q)
subformulas (p :<->: q) = (p :<->: q) : nub (subformulas p ++ subformulas q)

{-
wff1 == ["(P | Q) & P & Q","P | Q","P","Q","P & Q"]
wff2 == ["P & (Q | R) & (~P | ~Q) & (~P | ~R)","P & (Q | R)","P","Q | R","Q","R","(~P | ~Q) & (~P | ~R)","~P | ~Q","~P","~Q","~P | ~R","~R"]
wff3 == ["(P -> Q) & P & ~Q","P -> Q","P","Q","P & ~Q","~Q"]
wff4 == ["(P <-> Q) & (P & ~Q | ~P & Q)","P <-> Q","P","Q","P & ~Q | ~P & Q","P & ~Q","~Q","~P & Q","~P"]

It seems to be correct. Length of the lists above is always numOfFullTableCols + x where x is the number of unique ~Char
-}

-- 9.
wff5 :: Wff Atom
wff5 = ((V P :|: V Q) :&: (Not (V P) :&: Not (V Q)))
-- ((P ∨ Q) ∧ ((¬P) ∧ (¬Q)))
-- satisfiable wff5 == False
{- table wff5
P Q | (P | Q) & ~P & ~Q
- - | -----------------
F F |         F
F T |         F
T F |         F
T T |         F   
-}
-- tautology wff5 == False, tautology $ Not (wff5) == True

wff6 :: Wff Atom
wff6 = ((V P :->: V Q) :&: (V P :<->: V Q))
-- ((P → Q) ∧ (P ↔ Q))
-- satisfiable wff6 == True
{- table wff6
P Q | (P -> Q) & (P <-> Q)
- - | --------------------
F F |          T
F T |          F
T F |          F
T T |          T
-}

equivalent' :: Eq a => Wff a -> Wff a -> Bool
equivalent' p q = tautology newWff
 where newWff = (p :<->: q)

prop_equivalent :: Wff Atom -> Wff Atom -> Bool
prop_equivalent p q = equivalent p q == equivalent' p q 

-- e) It works correctly.

-- ** Optional Material

-- 10.
-- check for negation normal form

isNNF :: Wff a -> Bool
isNNF (V x) = True
isNNF (T) = True
isNNF (F) = True
isNNF (Not (T)) = True
isNNF (Not (F)) = True
isNNF (Not (V x)) = True
isNNF (p :|: q) = isNNF p && isNNF q -- ( a :<->: p)
isNNF (p :&: q) = isNNF p && isNNF q
isNNF _ = False

-- 11.
-- convert to negation normal form
impElim :: Wff a -> Wff a
impElim (Not (Not (V x))) = (V x)
impElim (Not (Not (p))) = (impElim p)
impElim (V x) = (V x)
impElim (T) = (T)
impElim (F) = (F)
impElim (Not (V x)) = (Not (V x))

impElim (p :|: q) = (impElim p :|: impElim q)
impElim (p :&: q) = (impElim p :&: impElim q)
impElim (p :->: q) = (Not (impElim p) :|: impElim q)
impElim (p :<->: q) = (impElim ((p :->: q) :&: (q :->: p)))

impElim (Not (p :|: q)) = (impElim (Not p) :&: impElim (Not q))
impElim (Not (p :&: q)) = (impElim (Not p) :|: impElim (Not q))

impElim (Not (p :->: q)) = (impElim (Not (Not (impElim p) :|: impElim q)))
impElim (Not (p :<->: q)) = (impElim (Not (impElim (p :<->: q))))


-- impElim (Not (V A :|: V B)) gives output ~A & ~B
-- for some reason it fails on codegrade with this type of input, but toNNF and both prop_NNF1 and prop_NNF2 work?
-- also impElim ~~A gives normally A, but ~(~A | B) gives ~~A & ~B, which is incorrect


toNNF :: Wff a -> Wff a
toNNF wff = if isNNF wff
          then wff
          else toNNF (impElim wff)

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff Atom -> Bool
prop_NNF1 f = isNNF (toNNF f)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Wff Atom -> Bool
prop_NNF2 f = equivalent f (toNNF f)

-- 12.
-- check whether a formula is in conj. normal form
isCNF :: Eq a => Wff a -> Bool
isCNF (V x) = True
isCNF (T) = True
isCNF (F) = True
isCNF (Not (Not (p))) = False
isCNF (Not (V x)) = True
isCNF (Not (_)) = False
-- codegrade doesn't seem to generate just T or F, but according to the pdf, T or F should not occur in wff normal forms
isCNF (T :|: q) = False
isCNF (p :|: T) = False
isCNF (F :|: q) = False
isCNF (p :|: F) = False
isCNF (T :&: q) = False
isCNF (p :&: T) = False
isCNF (F :&: q) = False
isCNF (p :&: F) = False

isCNF (p :|: q) = noInnerAnd p && noInnerAnd q && isCNF p && isCNF q -- First I thought noInnerAnd would be enough, but it has to check also if p and q isCNF -> (~~R | ~~Q)
isCNF (p :&: q) = isCNF p && isCNF q -- p and q should contain only (p :|: q) inside, but then it needs to check the lower branches using the pattern above
isCNF _ = False

noInnerAnd :: Wff a -> Bool
noInnerAnd (_ :&: _) = False
noInnerAnd (p :|: q) = noInnerAnd p && noInnerAnd q
noInnerAnd _ = True


-- 13.
-- transform a list of lists into a (CNF) formula
-- "Write a function listsToCNF to translate a list of lists of Wffs (which you may assume to be variables or negated variables) to a Wff in conjunctive normal form."

listsToCNF :: Eq a => [[Wff a]] -> Wff a
listsToCNF [] = (T) -- error "Invalid input"
listsToCNF [x] = if not (null x) then listToWff x else (F) -- error "Invalid input"
listsToCNF (x:xs) = containsTOrF (listToWff x :&: (listsToCNF xs))

listToWff :: Eq a => [Wff a] -> Wff a
listToWff [x] = x
listToWff (x:xs) = (x :|: (listToWff xs))
-- listsToCNF [[V A, V B],[V C,V D,V Q],[Not (V P)]] == (A | B) & (C | D | Q) & ~P

-- Since the input should be only lists of V x or Not (V x), the function below is unnecesary,
-- but I added it before I realised that. Now empty lists automatically returns T, but empty list in a list of Wff a outputs (F)
-- so for example [[V A, V B], [V C, V D], []] == (A | B) & (C | D) & F == F
-- if the lists in the lists would consist truly only of variables, then empty list and list of empty lists would just give an error

containsTOrF :: Wff a -> Wff a
containsTOrF (V x) = (V x)
containsTOrF (Not (V x)) = (Not (V x))
containsTOrF (T) = (T)
containsTOrF (F) = (F)
containsTOrF (T :|: q) = (T)
containsTOrF (p :|: T) = (T)
containsTOrF (F :|: q) = (q)
containsTOrF (p :|: F) = (p)
containsTOrF (T :&: q) = (q)
containsTOrF (p :&: T) = (p)
containsTOrF (F :&: q) = (F)
containsTOrF (p :&: F) = (F)
containsTOrF (p :&: q) = (containsTOrF p :&: containsTOrF q)
containsTOrF (p :|: q) = (containsTOrF p :|: containsTOrF q)
--containsTOrF (p :->: q) = (Not ( containsTOrF p) :|: containsTOrF q)
--containsTOrF (p :<->: q) = (containsTOrF ((p :->: q) :&: (q :->: p)))

-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Eq a => Wff a -> [[Wff a]]
listsFromCNF (Not (V x)) = listFromWff (Not (V x)) : []
listsFromCNF (V x) = listFromWff (V x) : []
listsFromCNF (T) = []
listsFromCNF (F) = [[]]
listsFromCNF (p :|: q) = (listFromWff p ++ listFromWff q) : []
listsFromCNF (p :&: q) = listsFromCNF p ++ listsFromCNF q

listFromWff :: Eq a => Wff a -> [Wff a]
listFromWff (Not (V x)) = [Not (V x)]
listFromWff (V x) = [V x]
listFromWff (p :|: q) = listFromWff p ++ listFromWff q

-- I am done...

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Eq a => Wff a -> [[Wff a]]
toCNFList = undefined

-- convert to conjunctive normal form
toCNF :: Eq a => Wff a -> Wff a
toCNF = listsToCNF . toCNFList

-- check if result of toCNF is in con. normal form
prop_CNF1 :: Wff Atom -> Bool
prop_CNF1 f = undefined

-- check if result of toCNF is equivalent to its input
prop_CNF2 :: Wff Atom -> Bool
prop_CNF2 p = undefined


-- ** PRESENTATION
showWff :: Show a => Wff a -> String
showWff e = showsPrec 0 e ""
  where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar 'F'
    showsPrec _  T    = showChar 'T'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showString "&" . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showString "|" . showSpace . 
          showsPrec 2 b )
    showsPrec _ (Not a) = 
      showString "~" . showsPrec 11 a
    showsPrec p (a :->: b)
      = showParen (p>1)
          (showsPrec 1 a .showSpace .
          showString "->" . showSpace . 
          showsPrec 1 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
          (showsPrec 0 a .showSpace .
          showString "<->" . showSpace . 
          showsPrec 0 b )
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s


-- ** For Drawing Tables

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s = replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s = replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False = "F"
fort True  = "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab = putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
  where
    widths  = map length (head tab)

table p = tables [p]

tables :: (Eq a, Show a) => [Wff a] -> IO ()
tables ps  =
  let xs = nub (concatMap atoms ps) in
   showTable (
     [ map show xs ++ ["|"] ++ [show p | p <- ps]           ] ++
     [ dashvars xs ++ ["|"] ++ [dash (show p) | p <- ps ]   ] 
     ++   [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
     )
  where  dashvars xs   = [ dash (show x) | x <- xs ]
         evalvars e xs = [ fort (eval e (V x)) | x <- xs ]

 -- print a truth table, including columns for subformulas
fullTable :: (Eq a, Show a) => Wff a -> IO ()
fullTable = tables . filter nontrivial . sortOn (length.atoms) . subformulas
  where nontrivial :: Wff a -> Bool
        nontrivial (Not(V _))   = False
        nontrivial (V _)        = False
        nontrivial T            = False
        nontrivial F            = False
        nontrivial _            = True


-- ** For QuickCheck
 
instance Show a => Show (Wff a) where
  show = showWff

instance Arbitrary Atom where
  arbitrary = oneof $ map return [ A, B, C, D, W, X, Y, Z]

instance Arbitrary a => Arbitrary (Wff a) where
  arbitrary = sized wff
      where
        wff n | n <= 0    = liftM V atom
              | otherwise = oneof [ liftM V atom
                                     , liftM Not subform
                                     , liftM2 (:|:) subform subform
                                     , liftM2 (:&:) subform subform
                                   --  , liftM2 (:->:) subform subform
                                   --  , liftM2 (:<->:)subform' subform'
                                     ]
               where
                 atom = Test.QuickCheck.arbitrary
                 subform = wff (n `div` 2)
                 subform' =  wff (n `div` 4)
