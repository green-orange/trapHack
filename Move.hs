module Move where

import Data
import Changes
import Utils4mon
import HealDamage
import Utils4objects
import Parts
import Messages
import Colors

import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import System.Random (randomR)

moveFirst :: Int -> Int -> World -> World
moveFirst dx dy world =
	if (isEmpty world xnew ynew) || (dx == 0 && dy == 0) || (rez == Nothing)
	then
		if (name mon) /= "You" && not (isFlying mon) 
			&& worldmap world !! x !! y == bEARTRAP
		then world
		else changeGen g'' $ changeMoveFirst xnew ynew 
			$ addMessage (newMessage, yELLOW) $ world
	else attacks xnew ynew world $ map (\p -> objectKeys p !! fromEnum WeaponSlot) 
		$ filter isUpperLimb $ parts $ getFirst world
	where
		x = xFirst world
		y = yFirst world
		rez = 
			if q <= tele
			then Just (xR, yR)
			else dirs world (x, y, dx, dy)
		(xnew, ynew, newMessage) =
			if rez == Nothing
			then
				if isPlayerNow world
				then (x, y, "Incorrect step!")
				else (x, y, "")
			else (xnew', ynew', "") where
				Just (xnew', ynew') = rez
		mon = getFirst world
		tele = intr mon !! fromEnum Teleport
		(q, g) = randomR (1, 100) $ stdgen world
		(xR, g') = randomR (0, maxX) g
		(yR, g'') = randomR (0, maxY) g'

attacks :: Int -> Int -> World -> [Char] -> World
attacks x y world links = foldr ($) world $ map (\c w -> attack x y c w) $ links

attack :: Int -> Int -> Char -> World -> World
attack x y c world = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' world
	where
		attacker = getFirst world
		mon = units world M.! (x, y)
		color = 
			if isPlayerNow world
			then case newDmg of
				Nothing -> cYAN
				_		-> gREEN
			else case ai mon of
				You ->
					case newDmg of
						Nothing -> yELLOW
						_		-> rED
				_ -> bLUE
		weapons = M.lookup c (inv attacker)
		dmggen = 
			if isNothing weapons || (not $ isWeapon $ fst $ fromJust weapons)
			then stddmg attacker
			else objdmg $ fst $ fromJust weapons
		(newDmg, newGen) =  dmggen world
		newMsg = case newDmg of
			Nothing -> (name attacker) ++ " missed!"
			Just _ -> (name attacker) ++ " attack" ++ (ending world) 
				++ (name mon) ++ "!"
		(monNew, newGen') = dmgRandom newDmg mon newGen
		unitsNew = changeList (M.insert (x, y) monNew $ units world) $ units' world
		
stupidestAI :: AIfunc
stupidestAI xPlayer yPlayer world = 
	newWorld
	where
		xNow = xFirst world
		yNow = yFirst world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		newWorld = moveFirst dx dy world

attackElem :: Elem -> Int -> Int -> World -> World
attackElem elem' dx dy w = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' w where
	attacker = getFirst w
	xNow = xFirst w
	yNow = yFirst w
	xNew = xNow + dx
	yNew = yNow + dy
	mon = units w M.! (xNew, yNew)
	color = case ai mon of
		You -> case newDmg of
			Nothing -> yELLOW
			_		-> rED
		_ -> bLUE
	dmggen = stddmg attacker
	(newDmg, newGen) =  dmggen w
	newMsg = case newDmg of
		Nothing -> (name attacker) ++ " missed!"
		Just _ -> (name attacker) ++ " " ++ (attackName elem') 
			++ ending w ++ (name mon) ++ "!"
	(monNew, newGen') = dmgRandomElem elem' newDmg mon newGen
	unitsNew = changeList (M.insert (xNew, yNew) monNew $ units w) $ units' w
