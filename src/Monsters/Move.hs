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
import Data.Maybe (fromMaybe)
import System.Random (randomR)

-- | move the current monster with given direction
moveFirst :: Int -> Int -> World -> (World, Bool)
moveFirst dx dy world
	| not $ isCell (x + dx) (y + dy)
		= (addNeutralMessage msgIncStep world, True)
	| not (isEmpty world xNew yNew) && (dx /= 0 || dy /= 0)
		= (maybeUpgrade xNew yNew $ foldr (attack xNew yNew . 
		(\ p -> objectKeys p !! fromEnum WeaponSlot))
		world $ filter isUpperLimb $ parts $ getFirst world, True)
	| terrain (worldmap world A.! (x + dx, y + dy)) == Water 
		&& not (isFlying mon) = (maybeAddMessage msgWater world, False)
	| name mon /= "You" && not (isFlying mon) 
		&& terrain (worldmap world A.! (x,y)) == BearTrap = (world, True)
	| heiNew > heiOld + 1 && not (isFlying mon) && q > tele =
		(maybeAddMessage msgTooHigh world {stdgen = g''}, False) 
	| otherwise = (dmgFallFirst (if tele >= q then 0 
		else heiOld - heiNew) $ changeMoveFirst xNew yNew $ addNeutralMessage 
		teleMsg world {stdgen = g''}, True)
	where
		x = xFirst world
		y = yFirst world
		(xNew, yNew, teleMsg) = 
			if q <= tele
			then (xR, yR, msgTeleport $ name mon)
			else (x + dx, y + dy, "")
		mon = getFirst world
		tele = intr mon !! fromEnum Teleport
		(q, g) = randomR (1, 100) $ stdgen world
		(xR, g') = randomR (0, maxX) g
		(yR, g'') = randomR (0, maxY) g'
		heiOld = height $ worldmap world A.! (x,y)
		heiNew = height $ worldmap world A.! (xNew, yNew)

-- | attack monster with giben coords and inventory index of the weapon
attack :: Int -> Int -> Char -> World -> World
attack x y c world = addMessage (newMsg, color) 
	world {action = Move, units' = unitsNew, stdgen = newGen'} where
	attacker = getFirst world
	mon = fromMaybe (putWE "attack") $ M.lookup (x, y) $ units world
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
		let withoutWeapons = uncurry dices $ stddmg attacker in
		case weapons of
			Nothing -> withoutWeapons
			Just (weap, _) -> 
				if not (isWeapon weap)
				then withoutWeapons
				else objdmg weap
	(newDmg, newGen) =  dmggen world
	newMsg = case newDmg of
		Nothing -> name attacker ++ msgMiss
		Just _ -> name attacker ++ msgAttack ++ ending world
			++ name mon ++ "!"
	(monNew, newGen') = dmgRandom newDmg mon newGen
	unitsNew = (units' world) {list = M.insert (x, y) monNew $ units world}

-- | maybe upgrade first monster after killing a monster with given coords
maybeUpgrade :: Int -> Int -> World -> World
maybeUpgrade x y w = changeMon monFirst $ addLevelUpMessages w {stdgen = gen} where
	attacker = getFirst w
	mon = fromMaybe (putWE "maybeUpgrade") $ M.lookup (x, y) $ units w
	(monFirst, lvls, gen) = if alive mon then (attacker, 0, stdgen w) else
		xpUp (stdgen w) attacker mon
	addLevelUpMessages = foldr (.) id $ replicate lvls 
		$ addNeutralMessage $ msgLevelUp $ name attacker

-- | attack with elemental power and given direction
attackElem :: Elem -> Int -> Int -> World -> World
attackElem elem' dx dy w = addMessage (newMsg, color) 
	w {action = Move, units' = unitsNew, stdgen = newGen'} where
	attacker = getFirst w
	xNow = xFirst w
	yNow = yFirst w
	xNew = xNow + dx
	yNew = yNow + dy
	mon = fromMaybe (putWE "attackElem") 
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
	unitsNew = (units' w) {list = M.insert (xNew, yNew) monNew $ units w}
