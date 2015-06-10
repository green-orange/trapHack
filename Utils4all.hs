module Utils4all where

import Data

import UI.HSCurses.Curses (Key(..))

bODY = 0 :: Int
hEAD = 1 :: Int
lEG  = 2 :: Int
aRM  = 3 :: Int
wING = 4 :: Int
pAW  = 5 :: Int
kINDS = pAW

mAIN = 32 :: Int

eMPTY    = 0 :: Int
bEARTRAP = 1 :: Int
fIRETRAP = 2 :: Int
tRAPSNUM = fIRETRAP

dEFAULT	   = tRAPSNUM + 1
gREEN	   = tRAPSNUM + 2
yELLOW	   = tRAPSNUM + 3
rED 	   = tRAPSNUM + 4
rEDiNVERSE = tRAPSNUM + 5
cYAN	   = tRAPSNUM + 6
mAGENTA	   = tRAPSNUM + 7
bLUE       = tRAPSNUM + 8

effectiveSlowness :: Monster -> Int
effectiveSlowness mon =
	div (slowness mon) $ 1 + (length $ filter isLowerLimb $ parts mon)


isLowerLimb :: Part -> Bool
isLowerLimb p = (kind p == lEG) || (kind p == wING) || (kind p == pAW)

isUpperLimb :: Part -> Bool
isUpperLimb p = (kind p == aRM) || (kind p == wING) || 
	(kind p == pAW) || (kind p == mAIN)

getFirst :: World -> Monster
getFirst world =
	if null $ units world
	then error "No monsters"
	else third $ head $ units world
	
first :: (a,b,c) -> a
first (x,_,_) = x

second :: (x,b,c) -> b
second (_,x,_) = x

third :: (a,b,c) -> c
third (_,_,x) = x
	
partToStr :: Int -> String
partToStr x
	| x == bODY = "Body"
	| x == hEAD = "Head"
	| x == lEG  = "Leg"
	| x == aRM  = "Arm"
	| x == wING = "Wing"
	| x == pAW  = "Paw"
	| x == mAIN = "Main"
	| otherwise = error "unknown part"

titleShow :: Object -> String
titleShow (Wand title _ _ charge) = title ++ " (" ++ show charge ++ ")"
titleShow x = title x

isStackable :: Object -> Bool
isStackable obj = obj == obj

isPotion :: Object -> Bool
isPotion (Potion _ _) = True
isPotion _ = False

isScroll :: Object -> Bool
isScroll (Scroll _ _) = True
isScroll _ = False

isWand :: Object -> Bool
isWand (Wand _ _ _ _) = True
isWand _ = False

isTrap :: Object -> Bool
isTrap (Trap _ _) = True
isTrap _ = False

isLauncher :: Object -> Bool
isLauncher (Launcher _ _ _) = True
isLauncher _ = False

isWeapon :: Object -> Bool
isWeapon (Weapon _ _) = True
isWeapon _ = False

isMissile :: Object -> Bool
isMissile (Missile _ _ _) = True
isMissile _ = False

fromKey :: Key -> Char
fromKey (KeyChar c) = c
fromKey _ = ' '

numToStr :: Int -> String
numToStr t
	| t == 1 = "first"
	| t == 2 = "second"
	| t == 3 = "third"
	| t == 4 = "forth"
	| t == 5 = "fifth"
	| t == 6 = "sixth"
	| t == 7 = "seventh"
	| t == 8 = "eighth"
	| t == 9 = "ninth"
	| t == 10 = "tenth"
	| t == 11 = "eleventh"
	| t == 12 = "twelfth"
	| t == 13 = "thirteenth"
	| t == 14 = "fourteenth"
	| t == 15 = "fifteenth"
	| t == 16 = "sixteenth"
	| t == 17 = "seventeenth"
	| t == 18 = "eighteenth"
	| t == 19 = "nineteenth"
	| t == 20 = "twentieth"
	| t == 30 = "thirtieth"
	| t == 40 = "fortieth"
	| t == 50 = "fiftieth"
	| t == 60 = "sixtieth"
	| t == 70 = "seventieth"
	| t == 80 = "eigthieth"
	| t == 90 = "ninetieth"
	| t < 100 = decToStr (div t 10) ++ "-" ++ numToStr (mod t 10)
	| otherwise = show t
	
decToStr :: Int -> String
decToStr t
	| t == 2 = "twenty"
	| t == 3 = "thirty"
	| t == 4 = "forty"
	| t == 5 = "fifty"
	| t == 6 = "sixty"
	| t == 7 = "seventy"
	| t == 8 = "eighty"
	| t == 9 = "ninety"
	| otherwise = error "wrong number of tens"

revAlphabet :: Char -> Int
revAlphabet c =
	if (c >= 'a' && c <= 'z')
	then (fromEnum c) - (fromEnum 'a')
	else (fromEnum c) - (fromEnum 'A') + (fromEnum 'z') - (fromEnum 'a') + 1

addIndices :: (a -> Bool) -> [a] -> [(a, Int)]
addIndices = addIndices' 0 where
	addIndices' :: Int -> (a -> Bool) -> [a] -> [(a, Int)]
	addIndices' _ _ [] = []
	addIndices' n f (x:xs) =
		if f x
		then (x, n) : addIndices' (n + 1) f xs
		else (x, -1) : addIndices' n f xs

split :: (a -> Bool) -> [a] -> ([a], [a])
split f [] = ([], [])
split f (x:xs) =
	if f x
	then (x:a, b)
	else (a, x:b)
	where (a, b) = split f xs

rectdirs :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe (Int, Int)
rectdirs (xmin, ymin, xmax, ymax) (x, y, dx, dy) =
	if (xnew >= xmin && xnew <= xmax && ynew >= ymin && ynew <= ymax)
	then Just (xnew, ynew)
	else Nothing
	where
		xnew = x + dx
		ynew = y + dy


isEmpty :: World -> Int -> Int -> Bool
isEmpty world x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY &&
	(not $ elem (x, y) [ (a, b) | (a, b, _) <- units world ])

isValid :: World -> Int -> Int -> Int -> Int -> Bool
isValid world x y dx dy = 
	case rez of
	Nothing -> False
	Just (x', y') -> isEmpty world x' y'
	where rez = dirs world (x, y, dx, dy)

cycle' :: [a] -> [a]
cycle' [] = []
cycle' (x:xs) = xs ++ [x]

dir :: Key -> Maybe (Int, Int)
dir c = case c of
	KeyChar 'k' -> Just ( 0, -1)
	KeyChar 'j' -> Just ( 0,  1)
	KeyChar 'h' -> Just (-1,  0)
	KeyChar 'l' -> Just ( 1,  0)
	KeyChar 'y' -> Just (-1, -1)
	KeyChar 'u' -> Just ( 1, -1)
	KeyChar 'b' -> Just (-1,  1)
	KeyChar 'n' -> Just ( 1,  1)
	KeyChar '.' -> Just ( 0,  0)
	_           -> Nothing
	
undir :: Int -> Int -> Key
undir   0  (-1) = KeyChar 'k'
undir   0    1  = KeyChar 'j'
undir (-1)   0  = KeyChar 'h'
undir   1    0  = KeyChar 'l'
undir (-1) (-1) = KeyChar 'y'
undir   1  (-1) = KeyChar 'u'
undir (-1)   1  = KeyChar 'b'
undir   1    1  = KeyChar 'n'
undir   0    0  = KeyChar '.'

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toEnum (fromEnum x - fromEnum 'a' + fromEnum 'A') : xs

ending :: World -> String
ending world =
	if isPlayerNow world
	then " "
	else "s "

isPlayerNow :: World -> Bool
isPlayerNow world = (name $ getFirst world) == "You" && (time $ getFirst world) == 0
