module GarbageCollector where

import Data
import Monsters
import Random
import Move
import Changes
import ObjectOverall
import Parts

import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import UI.HSCurses.Curses (Key (..))
import Data.Map (empty)

collectorAI :: AIfunc
collectorAI world _ _ = 
	if isItemHere
	then fromJust $ fst $ pickFirst $ foldr ($) world $ map (changePickFirst . KeyChar) alphabet
	else moveFirst dx dy world
	where
		isItemHere = not $ null $ filter (\(x, y, _, _) -> x == xNow && y == yNow) $ items world
		(xNow, yNow, _) = head $ units world
		(xItem, yItem, _, _) = minimumBy cmp $ items world
		dist x y = max (x - xNow) (y - yNow)
		cmp = on compare (\(x, y, _, _) -> dist x y)
		(dx, dy) = 
			if null $ items world
			then (0, 0)
			else (signum $ xItem - xNow,
				  signum $ yItem - yNow)
				  
getGarbageCollector = getMonster collectorAI
	[getBody 1 30, 
	 getHead 1 20,
	 getLeg  1 10,
	 getLeg  1 10,
	 getArm  1 10,
	 getArm  1 10]
	 "Garbage collector" (dices (2,4) 0.4) (const empty) 100
