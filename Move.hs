module Move where

import Data
import Changes
import Utils4mon
import HealDamage
import Utils4objects

import Prelude hiding (lookup)
import System.Random (StdGen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

moveFirst :: Int -> Int -> World -> World
moveFirst dx dy world =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name mon) /= "You" && not (isFlying mon) && worldmap world !! x !! y == bEARTRAP
		then world
		else
			changeMoveFirst xnew ynew $ addMessage (newMessage, yELLOW) $ world
	else
		attacks xnew ynew world $ countUpperLimbs $ getFirst world
	where
		x = xFirst world
		y = yFirst world
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

attacks :: Int -> Int -> World -> Int -> World
attacks x y world n = foldr ($) world $ replicate n $ (\w -> attack x y w)

attack :: Int -> Int -> World -> World
attack x y world = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' world
	where
		attacker = getFirst world
		mon = units world M.! (x, y)
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
		unitsNew = changeList (M.insert (x, y) monNew $ units world) $ units' world
		
stupidestAI :: AIfunc
stupidestAI world xPlayer yPlayer = 
	newWorld
	where
		xNow = xFirst world
		yNow = yFirst world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		newWorld = moveFirst dx dy world

