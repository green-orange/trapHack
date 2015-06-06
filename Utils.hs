module Utils where

import Data
import Stuff
import Utils4stuff
import Changes
import Utils4all
import Utils4mon

import System.Random (StdGen)
import Control.Monad ((>=>))

moveFirst :: World -> Int -> Int -> World
moveFirst world dx dy =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name mon) /= "You" && not (isFlying mon) && worldmap world !! x !! y == bEARTRAP
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
		mon = getFirst world

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
cycleWorld w = actTrapFirst $ regFirst $ cleanFirst $ changeMons newUnits 
	$ addMessage (msgCleanParts $ third $ head newUnits) w
	where newUnits = cycle' $ units w

cleanFirst :: World -> World
cleanFirst w = changeMon (cleanParts $ getFirst w) w

ending :: World -> String
ending world =
	if isPlayerNow world
	then " "
	else "s "

addInvs :: [(Char, Object, Int)] -> [(Object, Int)] -> Maybe [(Char, Object, Int)]
addInvs startInv items = (foldl (>=>) return $ map addInv items) startInv

addInv :: (Object, Int) -> [(Char, Object, Int)] -> Maybe [(Char, Object, Int)]
addInv (obj, cnt) list  =
	if null this
	then addInvWithAlphabet alphabet list (obj,cnt)
	else Just $ map change list
	where
		addInvWithAlphabet :: [Char] -> [(Char, Object, Int)] -> (Object, Int) -> Maybe [(Char, Object, Int)]
		addInvWithAlphabet [] _ _ = Nothing
		addInvWithAlphabet alph inv (obj, cnt) = 
			if length this == 0
			then Just $ (head alph, obj, cnt) : inv
			else addInvWithAlphabet (tail alph) inv (obj, cnt) where
				this = filter (\(x, _, _) -> x == head alph) inv
		this = filter (\(_,obj',_) -> obj' == obj) list
		change (c, o, n) = 
			if o == obj
			then (c, o, n + cnt)
			else (c, o, n)


remFirst :: World -> World
remFirst world = changeMons (tail $ units world) $ changeAction ' ' world

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
	time = time mon,
	weapon = weapon mon
}, newGen) where (newDrop, newGen) = deathDrop (name mon) g

tickDown :: World -> World
tickDown w = changeMon (tickDownMon $ getFirst w) w

resetTime :: World -> World
resetTime w = changeMon (resetTimeMon $ getFirst w) w

