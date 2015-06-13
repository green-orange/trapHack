module Move where

import Data
import Changes
import Utils4all
import Utils4mon
import HealDamage

import Prelude hiding (lookup)
import System.Random (StdGen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

moveFirst :: World -> Int -> Int -> World
moveFirst world dx dy =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name mon) /= "You" && not (isFlying mon) && worldmap world !! x !! y == bEARTRAP
		then world
		else
			changeMons ((xnew, ynew, getFirst world) 
			: (tail $ units world)) $ addMessage (newMessage, yELLOW) $ world
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
attack world x y = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' world
	where
		attacker = getFirst world
		found :: Unit -> Bool
		found (x', y', _) = (x' == x) && (y' == y)
		(xx,yy,mon) = head $ filter found $ units world
		color = 
			if name attacker == "You"
			then case newDmg of
				Nothing -> cYAN
				_		-> gREEN
			else if name mon == "You"
			then case newDmg of
				Nothing -> yELLOW
				_		-> rED
			else bLUE
		change :: Unit -> Unit -> Unit
		change tnew t = if found t then tnew else t
		weapons = M.lookup (weapon attacker) (inv attacker)
		dmggen = 
			if isNothing weapons || (not $ isWeapon $ fst $ fromJust weapons)
			then stddmg attacker
			else objdmg $ fst $ fromJust weapons
		(newDmg, newGen) =  dmggen world
		newMsg = case newDmg of
			Nothing -> (name attacker) ++ " missed!"
			Just _ -> (name attacker) ++ " attacks " ++ (name mon) ++ "!"
		(monNew, newGen') = dmgRandom newDmg mon newGen
		unitsNew = map (change (xx,yy,monNew)) $ units world
		
stupidestAI :: AIfunc
stupidestAI world xPlayer yPlayer = 
	newWorld
	where
		(xNow, yNow, _) = head $ units world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		newWorld = moveFirst world dx dy

