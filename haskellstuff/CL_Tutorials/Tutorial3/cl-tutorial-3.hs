data Thing = A | B | C | D | E deriving (Eq,Show)

things :: [Thing]
things = [ A, B, C, D, E ]

data Colour = Amber | Blue deriving Eq

colour :: Thing -> Colour
colour A = Amber
colour B = Amber
colour C = Amber
colour D = Blue
colour E = Amber
            
data Shape = Square | Disc deriving Eq

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Disc
shape D = Square
shape E = Square
           
data Size = Big | Small deriving Eq

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small
       
data Border = Thin | Thick deriving Eq

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

type Predicate u = u -> Bool

isAmber :: Predicate Thing
isAmber x = colour x == Amber

isBlue :: Predicate Thing
isBlue x = colour x == Blue

isSquare :: Predicate Thing
isSquare x = shape x == Square

isDisc :: Predicate Thing
isDisc x = shape x == Disc

isBig :: Predicate Thing
isBig x = size x == Big

isSmall :: Predicate Thing
isSmall x = size x == Small

hasThinBorder :: Predicate Thing
hasThinBorder x = border x == Thin

hasThickBorder :: Predicate Thing
hasThickBorder x = border x == Thick

--------------------------------------
-- Your task is to replace 'undefined' with the definitions of the operators below.

{-
Exercise 4
1. isAmber, isBig |/= hasThickBorder
2. isSmall |/= not (isDisc)
3. isSmall, isSquare |= not (isAmber)
-}

(|=) :: Predicate Thing -> Predicate Thing -> Bool
a |= b = [ x | x <- things, a x] == [ y | y <- things, b y && a y]
-- isDisc |= isAmber
-- first list takes all things that are disc, second takes all things that are disc and amber
-- if both lists are the same, then all discs are indeed amber

(|/=) :: Predicate Thing -> Predicate Thing -> Bool
a |/= b = [ x | x <- things, a x] /= [ y | y <- things, b y && a y]
-- isSquare |/= isAmber
-- first list takes all things that are square [A, B, D, E], second takes all things that are square and amber [A, B, E]
-- if these lists are not identical, it means that some square cannot be amber - square D is blue

(||=) :: [Predicate Thing] -> Predicate Thing -> Bool
a ||= b = and [ b thing | thing <- things, check thing]
 where check thng = and [ pred thng | pred <- a]
-- isBlue, isSquare |= isBig
-- check thng checks, if all predicates in list "a" are true for the single thing
-- if they are, it means that thing satisfies all antecedents and we can now compare it with succedent
-- now if all things that satisfied antecedents also satisfy succedent, then all blue squares are big

neg :: Predicate u -> Predicate u
(neg a) x = not (a x)

(|:|) :: Predicate u -> Predicate u -> Predicate u
(a |:| b) x = a x || b x

(&:&) :: Predicate u -> Predicate u -> Predicate u
(a &:& b) x = a x && b x

{-
Exercise 6
1. False
2. True
3. True
4. False
5. True
6. True

-}

-- Combining our infix operators may lead to ambigous expressions. For example, 'a |:| b &:& c' can be read in two ways: either '(a |:| b) &:& c' or 'a |:| (b &:& c)'. We can always instruct Haskell which one to choose by adding paranthesis ourselves. 
-- But there are also ways of instructing Haskell how to disambiguate. The next lines are setting the precedence for our infix operators. To learn more about this, watch the video on Precedence from the CL week 3 list of videos (https://media.ed.ac.uk/media/1_25gncs98). It is not crucial you understand all this at the moment; we will remind you of fixity declarations later on during the course. In short, these lines guarantee a non-ambigous parsing of our code.
infixr 0 |=
infixr 0 |/=
infixr 0 ||=
infixr 2 |:|
infixr 3 &:&
