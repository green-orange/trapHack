module Monsters.Move where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Items
import Utils.Changes
import Utils.Monsters
import Utils.HealDamage
import Utils.Random
import Monsters.Parts
import IO.Messages
import IO.Colors
import IO.Texts

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Maybe (fromJust, isNothing, fromMaybe)
import System.Random (randomR)

moveFirst :: Int -> Int -> World -> World
moveFirst dx dy world
	| not $ isEmpty world xnew ynew || (dx == 0 && dy == 0) || isNothing rez
		= maybeUpgrade xnew ynew $ foldr (attack xnew ynew . 
		(\ p -> objectKeys p !! fromEnum WeaponSlot))
		world $ filter isUpperLimb $ parts $ getFirst world
	| name mon /= "You" && not (isFlying mon) 
		&& terrain (worldmap world A.! (x,y)) == BearTrap = world
	| heiNew > heiOld + 1 = changeGen g'' $ maybeAddMessage msgTooHigh world
	| otherwise = changeGen g'' $ dmgFallFirst (heiOld - heiNew) 
		$ changeMoveFirst xnew ynew $ addNeutralMessage teleMsg 
		$ addMessage (newMessage, yELLOW) world
	where
		x = xFirst world
		y = yFirst world
		(rez, teleMsg) = 
			if q <= tele
			then (Just (xR, yR), msgTeleport $ name mon)
			else (dirs world (x, y, dx, dy), "")
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
		heiOld = height $ worldmap world A.! (x,y)
		heiNew = height $ worldmap world A.! (xnew, ynew)

attack :: Int -> Int -> Char -> World -> World
attack x y c world = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction Move $ changeGen newGen' world where
	attacker = getFirst world
	mon = fromMaybe (error $ msgWE "attack") $ M.lookup (x, y) $ units world
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
	mon = fromMaybe (error $ msgWE "maybeUpgrade") $ M.lookup (x, y) $ units w
	(monFirst, lvls, gen) = if alive mon then (attacker, 0, stdgen w) else
		xpUp (stdgen w) attacker mon
	addLevelUpMessages = foldr (.) id $ replicate lvls 
		$ addNeutralMessage $ msgLevelUp $ name attacker

attackElem :: Elem -> Int -> Int -> World -> World
attackElem elem' dx dy w = changeMons unitsNew $ addMessage (newMsg, color) 
	$ changeAction Move $ changeGen newGen' w where
	attacker = getFirst w
	xNow = xFirst w
	yNow = yFirst w
	xNew = xNow + dx
	yNew = yNow + dy
	mon = fromMaybe (error $ msgWE "attackElem") 
		$ M.lookup (xNew, yNew) $ units w
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
