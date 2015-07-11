module Move where

import Data
import Changes
import Utils4mon
import HealDamage
import Utils4objects
import Parts
import Messages
import Colors
import Texts
import Random
import DataWorld
import DataMonster
import DataObject
import DataDef

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Maybe (fromJust, isNothing)
import System.Random (randomR)

moveFirst :: Int -> Int -> World -> World
moveFirst dx dy world =
	if isEmpty world xnew ynew || (dx == 0 && dy == 0) || isNothing rez
	then
		if name mon /= "You" && not (isFlying mon) 
			&& worldmap world A.! (x,y) == BearTrap
		then world
		else changeGen g'' $ changeMoveFirst xnew ynew 
			$ addMessage (newMessage, yELLOW) world
	else maybeUpgrade xnew ynew $ foldr (attack xnew ynew . 
		(\ p -> objectKeys p !! fromEnum WeaponSlot))
		world $ filter isUpperLimb $ parts $ getFirst world
	where
		x = xFirst world
		y = yFirst world
		rez = 
			if q <= tele
			then Just (xR, yR)
			else dirs world (x, y, dx, dy)
		(xnew, ynew, newMessage) =
			if isNothing rez
			then
				if isPlayerNow world
				then (x, y, msgIncStep)
				else (x, y, "")
			else (xnew', ynew', "") where
				Just (xnew', ynew') = rez
		mon = getFirst world
		tele = intr mon !! fromEnum Teleport
		(q, g) = randomR (1, 100) $ stdgen world
		(xR, g') = randomR (0, maxX) g
		(yR, g'') = randomR (0, maxY) g'

attack :: Int -> Int -> Char -> World -> World
attack x y c world = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' world where
	attacker = getFirst world
	mon = case M.lookup (x, y) $ units world of
		Nothing -> error $ msgWE "attack"
		Just m -> m
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
		if isNothing weapons || not (isWeapon $ fst $ fromJust weapons)
		then uncurry dices $ stddmg attacker
		else objdmg $ fst $ fromJust weapons
	(newDmg, newGen) =  dmggen world
	newMsg = case newDmg of
		Nothing -> name attacker ++ msgMiss
		Just _ -> name attacker ++ msgAttack ++ ending world
			++ name mon ++ "!"
	(monNew, newGen') = dmgRandom newDmg mon newGen
	unitsNew = changeList (M.insert (x, y) monNew $ units world) $ units' world

maybeUpgrade :: Int -> Int -> World -> World
maybeUpgrade x y w = changeGen gen $ changeMon monFirst $ addLevelUpMessages w where
	attacker = getFirst w
	mon = case M.lookup (x, y) $ units w of
		Nothing -> error $ msgWE "maybeUpgrade"
		Just m -> m
	(monFirst, lvls, gen) = if alive mon then (attacker, 0, stdgen w) else
		xpUp (stdgen w) attacker mon
	addLevelUpMessages = foldr (.) id $ replicate lvls 
		$ addNeutralMessage $ msgLevelUp $ name attacker

attackElem :: Elem -> Int -> Int -> World -> World
attackElem elem' dx dy w = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction ' ' $ changeGen newGen' w where
	attacker = getFirst w
	xNow = xFirst w
	yNow = yFirst w
	xNew = xNow + dx
	yNew = yNow + dy
	mon = case M.lookup (xNew, yNew) $ units w of
		Nothing -> error $ msgWE "attackElem"
		Just m -> m
	color = case ai mon of
		You -> case newDmg of
			Nothing -> yELLOW
			_		-> rED
		_ -> bLUE
	dmggen = uncurry dices $ stddmg attacker
	(newDmg, newGen) =  dmggen w
	newMsg = case newDmg of
		Nothing -> name attacker ++ msgMiss
		Just _ -> name attacker ++ " " ++ attackName elem'
			++ ending w ++ name mon ++ "!"
	(monNew, newGen') = dmgRandomElem elem' newDmg mon newGen
	unitsNew = changeList (M.insert (xNew, yNew) monNew $ units w) $ units' w
