module Move where

import Data
import Utils4stuff
import Changes
import Utils4all
import Utils4mon

import System.Random (StdGen)

moveFirst :: World -> Int -> Int -> World
moveFirst world dx dy =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name mon) /= "You" && not (isFlying mon) && worldmap world !! x !! y == bEARTRAP
		then world
		else
			changeMons ((xnew, ynew, changeCoords xnew ynew $ getFirst world) 
			: (tail $ units world)) $ addMessage newMessage $ world
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

