module Utils where

import Data
import Stuff
import Utils4stuff
import Changes
import Utils4all
import Utils4mon

import UI.HSCurses.Curses (Key(..))
import System.Random (StdGen(..), randomR)

moveFirst :: World -> Int -> Int -> World
moveFirst world dx dy =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name $ getFirst world) /= "You" && worldmap world !! x !! y == bEARTRAP
		then world
		else
			changeMons ((xnew, ynew,
			changeCoords (getFirst world) xnew ynew) : (tail $ units world)) $
			addMessage newMessage $ world
	else
		attacks world xnew ynew $ countUpperLimbs $ getFirst world
	where
		(x, y, _) = head $ units world
		rez = dirs world (x, y, dx, dy)
		(xnew, ynew, newMessage) =
			if rez == Nothing
			then
				if isPlayerNow world
				then (x, y, "Incorrect step!")
				else (x, y, "")
			else (xnew', ynew', "") where
				Just (xnew', ynew') = rez

attacks :: World -> Int -> Int -> Int -> World
attacks world x y n = foldr ($) world $ replicate n $ (\w -> attack w x y)

attack :: World -> Int -> Int -> World
attack world x y = changeMons unitsNew $ addMessage newMsg 
	$ changeAction ' ' $ changeGen newGen' world
	where
		found :: Unit -> Bool
		found (x', y', _) = (x' == x) && (y' == y)
		(xx,yy,mon) = head $ filter found $ units world
		change :: Unit -> Unit -> Unit
		change tnew t = if found t then tnew else t
		(newDmg, newGen) =  (stddmg $ getFirst world) world
		newMsg = case newDmg of
			Nothing -> (name $ getFirst world) ++ " missed!"
			Just _ -> (name $ getFirst world) ++ " attacks " ++ (name mon) ++ "!"
		(monNew, newGen') = dmgRandom newDmg mon newGen
		unitsNew = map (change (xx,yy,monNew)) $ units world

rectdirs :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Maybe (Int, Int)
rectdirs (xmin, ymin, xmax, ymax) (x, y, dx, dy) =
	if (xnew >= xmin && xnew <= xmax && ynew >= ymin && ynew <= ymax)
	then Just (xnew, ynew)
	else Nothing
	where
		xnew = x + dx
		ynew = y + dy


isEmpty :: World -> Int -> Int -> Bool
isEmpty world x y = not $ elem (x, y) [ (a, b) | (a, b, _) <- units world ]

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


isPlayerNow :: World -> Bool
isPlayerNow world = (name $ getFirst world) == "You" && (time $ getFirst world) == 0

coordsPlayer :: World -> (Int, Int)
coordsPlayer w =
	if null yous
	then (-1, -1)
	else getCoords $ head yous
	where
		getCoords (x,y,_) = (x,y)
		yous = filter (\(_,_,x) -> (name x == "You")) $ units w

cycleWorld :: World -> World
cycleWorld w = regFirst $ cleanFirst $ changeMons newUnits 
	$ addMessage (msgCleanParts $ third $ head newUnits) w
	where newUnits = cycle' $ units w

cleanFirst :: World -> World
cleanFirst w = changeMon (cleanParts $ getFirst w) w

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

ending :: World -> String
ending world =
	if isPlayerNow world
	then " "
	else "s "

addInvs :: [(Char, Object, Int)] -> [(Object, Int)] -> [(Char, Object, Int)]
addInvs startInv items = foldl addInv startInv items

addInv :: [(Char, Object, Int)] -> (Object, Int) -> [(Char, Object, Int)]
addInv = addInvWithAlphabet alphabet where
	addInvWithAlphabet :: [Char] -> [(Char, Object, Int)] -> (Object, Int) -> [(Char, Object, Int)]
	addInvWithAlphabet [] _ _ = error "Author is too lazy to fix this bug"
	addInvWithAlphabet alph inv (obj, cnt) =
		if length this == 0
		then (head alph, obj, cnt) : inv
		else addInvWithAlphabet (tail alph) inv (obj, cnt) where
			this = filter (\(x, _, _) -> x == head alph) inv


remFirst :: World -> World
remFirst world = changeMons (tail $ units world) $ changeAction ' ' world

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

	
fromKey :: Key -> Char
fromKey (KeyChar c) = c
fromKey _ = ' '

addDeathDrop :: Monster -> StdGen -> (Monster, StdGen)
addDeathDrop mon g = (Monster {
	ai = ai mon,
	parts = parts mon,
	x = x mon,
	y = y mon,
	name = name mon,
	stddmg = stddmg mon,
	inv = (inv mon) ++ newDrop,
	slowness = slowness mon,
	time = time mon
}, newGen) where (newDrop, newGen) = deathDrop (name mon) g

isPotion :: Object -> Bool
isPotion (Potion _ _) = True
isPotion _ = False

isWand :: Object -> Bool
isWand (Wand _ _ _ _) = True
isWand _ = False

isTrap :: Object -> Bool
isTrap (Trap _ _) = True
isTrap _ = False

tickDown :: World -> World
tickDown w = changeMon (tickDownMon $ getFirst w) w

resetTime :: World -> World
resetTime w = changeMon (resetTimeMon $ getFirst w) w

