module AI where

import Data
import Move
import Object
import ObjectOverall
import Utils4all
import Changes
import Utils4AI
import Parts

import System.Random
import Data.Maybe (fromJust)
import UI.HSCurses.Curses (Key (..))
import Data.Map (empty)

acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f w x y = f (changeMon newMon w) x y where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f w x y = 
	if any (<= 5) $ map hp $ filter (\x -> kind x == hEAD || kind x == bODY) 
		$ parts $ getFirst w
	then addMessage ("Troll turned into a rock", bLUE) $ changeMon rock w
	else f w x y
	
rock = Monster {
	ai = AI (\w _ _ -> w),
	parts = [getMain 0 500 0],
	name = "Rock",
	stddmg = lol,
	inv = empty,
	slowness = 10000,
	time = 10000,
	weapon = ' '
}

humanoidAI :: AIfunc -> AIfunc
humanoidAI = healAI . zapAttackAI . pickAI
		
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
	then fst $ wieldFirst (KeyChar $ launcherAI w) w
	else f w x y
	
hunterAI :: AIfunc -> AIfunc
hunterAI = wieldLauncherAI . fireAI
		
stupidAI :: AIfunc
stupidAI world xPlayer yPlayer = 
	newWorld
	where
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
		newWorld = moveFirst (changeGen newStdGen world) dx' dy'
				
randomAI :: AIfunc
randomAI world _ _  = (moveFirst newWorld rx ry) where
	g = stdgen world
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' world
