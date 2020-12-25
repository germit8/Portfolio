-- Exercise 1
-- I think that A is the odd one out, because it is the only one that does not stand out, every other thing has something unique to them

data Thing = A | B | C | D | E deriving (Eq, Show)
things :: [Thing]
things = [A, B, C, D, E]

data Colour = Orange | Blue deriving Eq
data Shape = Square | Disc deriving Eq
data Line = Thick | Thin deriving Eq
data Size = Big | Small deriving Eq

colour :: Thing -> Colour
colour D = Blue
colour _ = Orange

shape :: Thing -> Shape
shape C = Disc
shape _ = Square

line :: Thing -> Line
line B = Thin
line _ = Thick

size :: Thing -> Size
size E = Small
size _ = Big

type Predicate u = u -> Bool

isSmall :: Predicate Thing
isSmall x = size x == Small

isBig :: Predicate Thing
isBig x = size x /= Small -- not (isSmall x)

isThin :: Predicate Thing
isThin x = line x == Thin

isThick :: Predicate Thing
isThick x = line x /= Thin -- not (isThin x)

isDisc :: Predicate Thing
isDisc x = shape x == Disc

isSquare :: Predicate Thing
isSquare x = shape x /= Disc -- not (isDisc x)

isBlue :: Predicate Thing
isBlue x = colour x == Blue

isOrange :: Predicate Thing
isOrange x = colour x /= Blue-- not (isBlue x)

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan thing = [ x | x <- things, x /= thing]

properties :: [Predicate Thing]
properties = [isSmall, isBig, isThin, isThick, isDisc, isSquare, isBlue, isOrange]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf thing = [ predic | predic <- properties, (predic thing)]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing pred thing = or [ pred x | x <- thingsOtherThan thing]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf thing = [pred | pred <- properties, (isPropertyOfAnotherThing pred thing) == False && (pred thing)]

rank :: Thing -> Int
rank thing = length (propertiesOnlyOf thing) -- yep, A stands out - only with rank 0

{- 

Exercise 5

1. and [ isThin x | x <- things, isBlue x && isSquare x]
2. or [ not (isSquare x) | x <- things, isOrange x]
3. and [ isOrange x || isThick x | x <- things, isBig x && isSquare x]
4. or [ not (isBig x) | x <- things, isOrange x && isDisc x]

Exercise 6

1. not (or [ isBlue x | x <- things, isSquare x])
2. and [ not (isBlue x) | x <- things, isSquare x]

-}

-- or [ isOrange x | x <- things, isSquare x]
-- and [ isOrange x | x <- things]
-- 