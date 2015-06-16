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

import System.Random
import Data.Maybe (fromJust)
import UI.HSCurses.Curses (Key (..))
import Data.Map (empty, toList)

acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f w x y = f (changeMon newMon w) x y where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f w x y = 
	if any (<= 5) $ map hp $ filter (\x -> kind x == hEAD || kind x == bODY) 
		$ parts $ getFirst w
	then addMessage ("Troll turned into a rock.", bLUE) $ changeMon rock w
	else f w x y

rock = fst $ getMonster (\w _ _ -> w) [getMain 0 500] "Rock" lol (const empty) 10000 lol

humanoidAI :: AIfunc -> AIfunc
humanoidAI = healAI . zapAttackAI . wieldWeaponAI . useItemsAI . pickAI
		
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
		(xNow, yNow, _) = head $ units w
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		
pickAI :: AIfunc -> AIfunc
pickAI f w x y =
	if length objects > 0
	then fromJust $ fst $ pickFirst $ foldr ($) w $ map (changePickFirst . KeyChar) alphabet
	else f w x y where
		(xNow, yNow, _) = head $ units w
		objects = filter (\(x, y, _, _) -> x == xNow && y == yNow) $ items w
		
fireAI :: AIfunc -> AIfunc
fireAI ai world xPlayer yPlayer =
	if (canFire $ getFirst world) && isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI world) world
	else ai world xPlayer yPlayer
	where
		(xNow, yNow, _) = head $ units world
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
		invList = toList $ inv $ getFirst w
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
		(xNow, yNow, _) = head $ units w
		dx = x - xNow
		dy = y - yNow
	

stupidAI = stupidFooAI moveFirst
stupidParalysisAI = stupidFooAI (\x y w -> moveFirst x y $ paralyse x y w)

stupidFooAI :: (Int -> Int -> World -> World) -> AIfunc
stupidFooAI foo world xPlayer yPlayer = newWorld where
	g = stdgen world
	(xNow, yNow, _) = head $ units world
	dx = signum $ xPlayer - xNow
	dy = signum $ yPlayer - yNow
	(dx1, dy1, dx2, dy2) = 
		if dx == 0
		then (1, dy, -1, dy)
		else if dy == 0
		then (dx, 1, dx, -1)
		else (dx, 0, 0, dy)
	(dx', dy', newStdGen) = 
		if isValid world xNow yNow dx dy || 
			abs (xPlayer - xNow) <= 1 && abs (yPlayer - yNow) <= 1
		then (dx, dy, g)
		else if isValid world xNow yNow dx1 dy1
		then (dx1, dy1, g)
		else if isValid world xNow yNow dx2 dy2
		then (dx2, dy2, g)
		else 
			let
				(rx, g') = randomR (-1, 1) g
				(ry, g'') = randomR (-1, 1) g'
			in (rx, ry, g'')
	newWorld = foo dx' dy' $ changeGen newStdGen world
				
randomAI :: AIfunc
randomAI world _ _  = (moveFirst rx ry newWorld) where
	g = stdgen world
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' world
	
wormAI :: AIfunc
wormAI world xPlayer yPlayer = 
	if null mons
	then spawnMon tailWorm xNow yNow $ moveFirst dx dy world
	else if name mon == "Tail" || name mon == "Worm"
	then killFirst world
	else moveFirst dx dy world where
		(xNow, yNow, _) = head $ units world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		xNew = xNow + dx
		yNew = yNow + dy
		mons = filter (\(x, y, _) -> x == xNew && y == yNew) $ units world
		[(_,_,mon)] = mons
	
tailWorm = getMonster (\w _ _ -> w) [getMain 0 100] "Tail" lol (const empty) 10000

