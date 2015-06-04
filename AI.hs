module AI where

import Data
import Utils
import Object
import Utils4all
import Changes

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
	then fromJust $ pickFirst $ foldr ($) world $ map (changePickFirst . KeyChar) alphabet
	else ai world xPlayer yPlayer
	where
		(xNow, yNow, _) = head $ units world
		objects = filter (\(x, y, _, _) -> x == xNow && y == yNow) $ items world
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


needToBeHealedM :: Monster -> Bool
needToBeHealedM mon =
	foldl (||) False $ map (\x -> kind x == bODY && needToBeHealed x) $ parts mon

needToBeHealed :: Part -> Bool
needToBeHealed part = 3 * (hp part) < maxhp part

canBeHealed :: Monster -> Bool
canBeHealed mon = foldl (||) False $ map (isHealing . second) $ inv mon

isHealing :: Object -> Bool
isHealing obj = title obj == "potion of healing"

healAI :: World -> Char
healAI world = first $ head $ filter (isHealing . second) $ inv $ getFirst world

canZapToAttack :: Monster -> Bool
canZapToAttack mon = foldl (||) False $ map (isAttackWand . second) $ inv mon

isAttackWand :: Object -> Bool
isAttackWand obj = isWand obj && charge obj > 0 && title obj == "wand of striking"

zapAI :: World -> Char
zapAI world = first $ head $ filter (isAttackWand . second) $ inv $ getFirst world

isOnLine :: Int -> Int -> Int -> Int -> Int -> Bool
isOnLine d x1 y1 x2 y2 = abs (x1 - x2) <= d && abs (y1 - y2) <= d &&
	(x1 == x2 || y1 == y2 || x1 - y1 == x2 - y1 || x1 + y1 == x2 + y2)

