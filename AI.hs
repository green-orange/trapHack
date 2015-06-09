module AI where

import Data
import Move
import Object
import Utils4all
import Changes
import Utils4AI

import System.Random
import Data.Maybe (fromJust)
import UI.HSCurses.Curses (Key (..))

aiHumanoid :: AIfunc -> AIfunc
aiHumanoid ai world xPlayer yPlayer =
	if (canBeHealed $ getFirst world) && (needToBeHealedM $ getFirst world)
	then fst $ quaffFirst (KeyChar $ healAI world) world
	else if (canZapToAttack $ getFirst world) && isOnLine 5 xNow yNow xPlayer yPlayer
	then zapMon (undir dx dy) (zapAI world) world
	else if length objects > 0
	then fromJust $ fst $ pickFirst $ foldr ($) world $ map (changePickFirst . KeyChar) alphabet
	else ai world xPlayer yPlayer
	where
		(xNow, yNow, _) = head $ units world
		objects = filter (\(x, y, _, _) -> x == xNow && y == yNow) $ items world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		
aiHunter :: AIfunc -> AIfunc
aiHunter ai world xPlayer yPlayer =
	if (weapon $ getFirst world) == ' '
	then fst $ wieldFirst (KeyChar $ launcherAI world) world
	else if (canFire $ getFirst world) && isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI world) world
	else ai world xPlayer yPlayer
	where
		(xNow, yNow, _) = head $ units world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		
stupidestAI :: AIfunc -- monsters attack each other
stupidestAI world xPlayer yPlayer = 
	newWorld
	where
		(xNow, yNow, _) = head $ units world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		newWorld = moveFirst world dx dy
		
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
	
