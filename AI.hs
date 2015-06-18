module AI where

import Data
import Move
import Object
import ObjectOverall
import Changes
import Utils4AI
import Parts
import Monsters
import Utils4mon

import System.Random (randomR)
import Data.Maybe (fromJust, isNothing)
import UI.HSCurses.Curses (Key (..))
import qualified Data.Map as M

acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f w x y = f (changeMon newMon w) x y where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f w x y = 
	if any (<= 5) $ map hp $ filter (\p -> kind p == hEAD || kind p == bODY) 
		$ parts $ getFirst w
	then addMessage ("Troll turned into a rock.", bLUE) $ changeMon rock w
	else f w x y

rock :: Monster
rock = fst $ getMonster (\w _ _ -> w) [getMain 0 500] "Rock" lol (const M.empty) 10000 lol

humanoidAI :: AIfunc -> AIfunc
humanoidAI = healAI . zapAttackAI . wieldWeaponAI . useItemsAI . pickAI

mODSAI :: [AIfunc -> AIfunc]
mODSAI = [healAI, zapAttackAI, pickAI, fireAI, wieldLauncherAI, wieldWeaponAI, useItemsAI]
		
healAI :: AIfunc -> AIfunc
healAI f w x y = 
	if (canBeHealed $ getFirst w) && (needToBeHealedM $ getFirst w)
	then fst $ quaffFirst (KeyChar $ healingAI w) w
	else f w x y
	
zapAttackAI :: AIfunc -> AIfunc
zapAttackAI f w xPlayer yPlayer = 
	if (canZapToAttack $ getFirst w) && isOnLine 5 xNow yNow xPlayer yPlayer
	then zapMon (undir dx dy) (zapAI w) w
	else f w xPlayer yPlayer where
		xNow = xFirst w
		yNow = yFirst w
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		
pickAI :: AIfunc -> AIfunc
pickAI f w x y =
	if length objects > 0
	then fromJust $ fst $ pickFirst $ foldr ($) w $ map (changeChar . KeyChar) alphabet
	else f w x y where
		xNow = xFirst w
		yNow = yFirst w
		objects = filter (\(x', y', _, _) -> x' == xNow && y' == yNow) $ items w
		
fireAI :: AIfunc -> AIfunc
fireAI f w xPlayer yPlayer =
	if (canFire $ getFirst w) && isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI w) w
	else f w xPlayer yPlayer
	where
		xNow = xFirst w
		yNow = yFirst w
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		
wieldLauncherAI :: AIfunc -> AIfunc
wieldLauncherAI f w x y = 
	if (weapon $ getFirst w) == ' '
	then case launcherAI w of
		Nothing -> f w x y
		Just c -> fst $ wieldFirst (KeyChar c) w
	else f w x y
	
wieldWeaponAI :: AIfunc -> AIfunc
wieldWeaponAI f w x y = 
	if (weapon $ getFirst w) == ' '
	then case weaponAI w of
		Nothing -> f w x y
		Just c -> fst $ wieldFirst (KeyChar c) w
	else f w x y
	
useItemsAI :: AIfunc -> AIfunc
useItemsAI f w x y = case useSomeItem objs keys of
	Nothing -> f w x y
	Just g -> g w
	where
		invList = M.toList $ inv $ getFirst w
		objs = map (fst . snd) invList
		keys = map (KeyChar . fst) invList
	
hunterAI :: AIfunc -> AIfunc
hunterAI = wieldLauncherAI . fireAI

attackIfClose :: Int -> AIfunc -> AIfunc
attackIfClose dist f w x y =
	if abs dx <= dist && abs dy <= dist
	then moveFirst dx dy w
	else f w x y
	where
		xNow = xFirst w
		yNow = yFirst w
		dx = x - xNow
		dy = y - yNow
	
stupidAI, stupidParalysisAI :: AIfunc
stupidAI = stupidFooAI moveFirst
stupidParalysisAI = stupidFooAI (\x y w -> moveFirst x y $ paralyse x y w)

stupidFooAI :: (Int -> Int -> World -> World) -> AIfunc
stupidFooAI foo w xPlayer yPlayer = newWorld where
	g = stdgen w
	xNow = xFirst w
	yNow = yFirst w
	dx = signum $ xPlayer - xNow
	dy = signum $ yPlayer - yNow
	(dx1, dy1, dx2, dy2) = 
		if dx == 0
		then (1, dy, -1, dy)
		else if dy == 0
		then (dx, 1, dx, -1)
		else (dx, 0, 0, dy)
	(dx', dy', newStdGen) = 
		if isValid w xNow yNow dx dy || 
			abs (xPlayer - xNow) <= 1 && abs (yPlayer - yNow) <= 1
		then (dx, dy, g)
		else if isValid w xNow yNow dx1 dy1
		then (dx1, dy1, g)
		else if isValid w xNow yNow dx2 dy2
		then (dx2, dy2, g)
		else 
			let
				(rx, g') = randomR (-1, 1) g
				(ry, g'') = randomR (-1, 1) g'
			in (rx, ry, g'')
	newWorld = foo dx' dy' $ changeGen newStdGen w
				
randomAI :: AIfunc
randomAI w _ _  = (moveFirst rx ry newWorld) where
	g = stdgen w
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' w
	
wormAI :: AIfunc
wormAI w xPlayer yPlayer = 
	if isNothing maybeMon
	then spawnMon tailWorm xNow yNow $ moveFirst dx dy w
	else if name mon == "Tail" || name mon == "Worm"
	then killFirst w
	else moveFirst dx dy w where
		xNow = xFirst w
		yNow = yFirst w
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		xNew = xNow + dx
		yNew = yNow + dy
		maybeMon = M.lookup (xNew, yNew) (units w)
		Just mon = maybeMon

tailWorm :: MonsterGen
tailWorm = getMonster (\w _ _ -> w) [getMain 0 100] "Tail" lol (const M.empty) 10000

