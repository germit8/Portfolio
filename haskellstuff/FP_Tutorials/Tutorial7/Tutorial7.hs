module Tutorial7 where

import LSystem
import Test.QuickCheck
-- import Text.PrettyPrint.GenericPretty -- ** Uncomment for Generic Pretty Challenge

pathExample = spiral 0.1 1000 0.1 (-4)

-- 1a. split
split :: Command -> [Command]
split (Sit) = []
split (Go x) = [Go x]
split (Turn y) = [Turn y]
split (a :#: b) = split a ++ split b

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join xs = foldr1 (:#:) xs

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join $ split c) c

prop_split :: Command -> Bool
prop_split c = and [isOk x | x <- split c]
 where isOk :: Command -> Bool
       isOk (_ :#: _) = False
       isOk Sit = False
       isOk _ = True

-- 2a. copy
copy :: Int -> Command -> Command
copy i c | i <= 0    = Sit 
         | otherwise = join $ replicate i c

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
-- it fails with d = 0.0, but other than that it works normally, not sure what the base cases are supposed to be
polygon :: Distance -> Int -> Command
polygon 0 _ = Sit
polygon d i | i == 0    = Sit
            | i == 1    = (Go d) -- one line is not a polygon, so it should probably return error, but drawing one line as one side still kinda makes sense?
            | i == 2    = error "A polygon cannot have only 2 sides!"
            | otherwise = copy i (Go d :#: Turn (180.0 - oneOuterAngle))
 where oneOuterAngle = ((side - 2) * (180 / side))
       side = fromIntegral i


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral 0 n s a = Sit
spiral d 0 s a = Sit
spiral d n 0 a = copy n (Go d :#: Turn a)
spiral d n s a = (Go d :#: Turn a) :#: spiral (d+s) (n-1) s a


-- 4. optimise
-- Remember that Go does not take negative arguments.
optimise :: Command -> Command
optimise c = join $ filter satisfies (newCommand $ betterSplit c)
 where newCommand :: [Command] -> [Command]
       newCommand [] = []
       newCommand [x] = [x]
       newCommand (Go num1:Go num2:xs) = newCommand (Go (num1 + num2):xs)
       newCommand (Turn num1:Turn num2:xs) = newCommand (Turn (num1 + num2):xs)
       newCommand (a:b:xs) = a : newCommand (b:xs)

satisfies :: Command -> Bool
satisfies c = isTurn c || isGo c
 where
     isTurn (Turn x) | x /= 0    = True
                     | otherwise = False
     isTurn _                    = False
     isGo (Go x)     | x > 0     = True
                     | otherwise = False
     isGo _                      = False

-- split which ignores Go/Turn 0
betterSplit :: Command -> [Command]
betterSplit (Sit) = []
betterSplit (Go 0) = []
betterSplit (Turn 0) = []
betterSplit (Go x) = [Go x]
betterSplit (Turn y) = [Turn y]
betterSplit (a :#: b) = betterSplit a ++ betterSplit b

-- ** Optional Material

-- L-Systems

-- 5. arrowhead
{-
angle: 60
start: f
rewrite: f → g+f+g
g → f-g-f
-}
arrowhead :: Int -> Command
arrowhead i = f i
 where
     f 0 = GrabPen red :#: Go 10
     f i = g (i-1) :#: p :#: f (i-1) :#: p :#: g (i-1)
     g 0 = GrabPen blue :#: Go 10
     g i = f (i-1) :#: n :#: g (i-1) :#: n :#: f (i-1)
     n = Turn 60
     p = Turn (-60)

-- 6. snowflake
{-
angle: 60
start: f--f--f--
rewrite: f → f+f--f+f
-}
snowflake :: Int -> Command
snowflake i = f i :#: n :#: n :#: f i :#: n :#: n :#: f i :#: n :#: n
 where
     f 0 = Go 10
     f i = f (i-1) :#: p :#: f (i-1) :#: n :#: n :#: f (i-1) :#: p :#: f (i-1)
     p = Turn (-60)
     n = Turn 60

-- 7. hilbert
{-
angle: 90
start: l
rewrite: l → +rf-lfl-fr+
         r → -lf+rfr+fl
-}
-- does not pass with 0, but other than that it seems to work
-- for 0-3 it's not rotated as in pictures from wikipedia, I fixed that by starting with p :#: l i
-- but that is not in the original assignment
hilbert :: Int -> Command
--hilbert 0 = Sit
hilbert i = l i
 where
     l 0 = GrabPen red
     l i = p :#: r (i-1) :#: f :#: n :#: l (i-1) :#: f :#: l (i-1) :#: n :#: f :#: r (i-1) :#: p
     r 0 = GrabPen black
     r i = n :#: l (i-1) :#: f :#: p :#: r (i-1) :#: f :#: r (i-1) :#: p :#: f :#: l (i-1) :#: n
     f = Go 10
     p = Turn (-90)
     n = Turn 90


-- ** Challenge

-- Bonus L-Systems
{-
angle: 60
start: f
rewrite: f → f+g++g-f--ff-g+
         g → -f+gg++g+f--f-g
-}
peanoGosper :: Int -> Command
peanoGosper i = f i
 where
     f 0 = GrabPen red :#: Go 10
     f i = f (i-1) :#: p :#: g (i-1) :#: p :#: p :#: g (i-1) :#: n :#: f (i-1) :#: n :#: n :#: f (i-1) :#: f (i-1) :#: n :#: g (i-1) :#: p
     g 0 = GrabPen black :#: Go 10
     g i = n :#: f (i-1) :#: p :#: g (i-1) :#: g (i-1) :#: p :#: p :#: g (i-1) :#: p :#: f (i-1) :#: n :#: n :#: f (i-1) :#: n :#: g (i-1)
     p = Turn (-60)
     n = Turn 60

{-
angle: 90
start: f-f-f-f
rewrite: f → f-f+f+ff-f-f+f
-}
cross :: Int -> Command
cross i = f i :#: n :#: f i :#: n :#: f i :#: n :#: f i
 where
     f 0 = GrabPen green :#: Go 10
     f i = f (i-1) :#: n :#: f (i-1) :#: p :#: f (i-1) :#: p :#: f (i-1) :#: f (i-1) :#: n :#: f (i-1) :#: n :#: f (i-1) :#: p :#: f (i-1)
     p = Turn (-90)
     n = Turn 90

{-
angle: 22.5
start: g
rewrite: g → f-[[g]+g]+f[+fg]-g
         f → ff
-}
branch :: Int -> Command
branch i = g i
 where
     g 0 = GrabPen black :#: Go 10
     g i = f (i-1) :#: n :#: Branch (Branch (g (i-1)) :#: p :#: g (i-1)) :#: p :#: f (i-1) :#: Branch (p :#: f (i-1) :#: g (i-1)) :#: n :#: g (i-1)
     f 0 = GrabPen green :#: Go 10
     f i = f (i-1) :#: f (i-1)
     p = Turn (-22.5)
     n = Turn (22.5)

{-
angle: 90
start: F+F+F+F
rewrite: F → -F+F-F-F+F+FF-F+F+FF+F-F-FF+FF-FF+F+F-FF-F-F+FF-F-F+F+F-F+
-}
-- it works for 0 for me, but when I run it with higher number, my vs code crashes
thirtytwo :: Int -> Command
thirtytwo i = f i :#: p :#: f i :#: p :#:f i :#: p :#: f i
 where
     f 0 = GrabPen green :#: Go 10
     f i = iCantDoItManuallyAnymore myStr -- n :#: f (i-1) :#: p :#: f (i-1) :#: n :#: f (i-1) :#: n :#: f (i-1) :#: p :#: f (i-1) :#: p :#: f (i-1) :#: f (i-1) :#: n :#: f (i-1) :#: p :#: f (i-1) :#: p :#: f (i-1) :#: f (i-1) :#: p :#: f (i-1) :#: n :#: f (i-1) :#: n :#: f (i-1) :#: f (i-1) :#: p :#: f (i-1) :#: f (i-1) :#: n :#: f (i-1) :#: f (i-1) :#: p :#: f (i-1) :#: p :#: f (-1) :#: n :#: f (i-1) :#: f (i-1) :#: n :#: f (i-1) :#: n :#: f (i-1) :#: p :#: f (i-1) :#: f (i-1) :#: n :#: f (i-1) :#: n :#: f (i-1) :#: p :#: f (-1) :#: p :#: f (i-1) :#: n :#: f (i-1) :#: p
     p = Turn (-90)
     n = Turn 90

     myStr = "-F+F-F-F+F+FF-F+F+FF+F-F-FF+FF-FF+F+F-FF-F-F+FF-F-F+F+F-F+" -- string for rewriting
     -- function for automatically generating the command based on characters
     iCantDoItManuallyAnymore :: String -> Command
     iCantDoItManuallyAnymore "" = Sit
     iCantDoItManuallyAnymore (x:xs) | x == 'F'   = f (i-1) :#: iCantDoItManuallyAnymore xs
                                     | x == '+'   = p :#: iCantDoItManuallyAnymore xs
                                     | x == '-'   = n :#: iCantDoItManuallyAnymore xs
