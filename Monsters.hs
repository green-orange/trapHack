module Monsters where

import Data
import Random
import Utils4all
import Changes

import System.Random (StdGen, randomR)

getPart :: Int -> Int -> Int -> Int -> Part
getPart knd regVel hp id = Part {
	hp = hp,
	maxhp = hp,
	kind = knd,
	idP = id,
	regVel = regVel,
	aliveP = True
}

getBody = getPart bODY
getHead = getPart hEAD
getLeg  = getPart lEG
getArm  = getPart aRM
getWing = getPart wING
getPaw  = getPart pAW
getMain = getPart mAIN
	
getMonster :: AIfunc -> [Int -> Part] -> String -> StdDmg -> InvGen -> Int -> MonsterGen
getMonster ai ps name stddmg inv slow x y g = (Monster {
	ai = AI ai,
	parts = zipWith ($) ps [0..],
	name = name,
	stddmg = stddmg,
	inv = inv p,
	slowness = slow,
	time = slow,
	weapon = ' '
}, newGen) where
	p :: Float
	(p, newGen) = randomR (0.0, 1.0) g

getPlayer :: Int -> Int -> Monster
getPlayer x y = Monster {
	ai = You,
	parts = zipWith ($) 
		[getBody 1 40, 
		 getHead 1 30, 
		 getLeg  2 20, 
		 getLeg  2 20, 
		 getArm  2 20, 
		 getArm  2 20]
		 [0..],
	name = "You",
	stddmg = dices (1,10) 0.2,
	inv = [],
	slowness = 100,
	time = 100,
	weapon = ' '
}
		
getDummy n q = getMonster (\w _ _ -> w) [getMain 1 n] "Dummy" lol (const []) 100

addMonsters :: [MonsterGen] -> ([Unit], StdGen) -> ([Unit], StdGen)
addMonsters gens pair = foldr addMonster pair gens

addMonster :: MonsterGen -> ([Unit], StdGen) -> ([Unit], StdGen)
addMonster gen (units, g) = 
	if isCorrect
	then (units ++ [(x, y, mon)], g3)
	else addMonster gen (units, g3)
	where
	(x, g1) = randomR (0, maxX) g
	(y, g2) = randomR (0, maxX) g1
	(mon, g3) = gen x y g2
	isCorrect = 0 == length [(a,b) | (a,b,_) <- units, a == x, b == y]
	
animate :: Int -> Int -> World -> World
animate x y w = 
	if isEmpty w x y && hp > 0
	then spawnMon (getDummy hp lol) x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		mapfun arg@(_, _, _, n) = 
			if filterfun arg
			then n
			else 0
		hp = sum $ map mapfun $ items w
		newItems = filter (not . filterfun) $ items w
	
animateAround w = foldr ($) w $ [animate] >>= applToNear x >>= applToNear y where
	(x, y, _) = head $ units w
	applToNear x f = map f [x-1, x, x+1]
	
randomSpawn :: MonsterGen -> World -> World
randomSpawn mgen w = newWorld where
	(x, y, _) = head $ units w
	neighbors = [(x', y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]]
	emptyNeighbors = filter (uncurry $ isEmpty w) neighbors
	newWorld = 
		if null emptyNeighbors
		then maybeAddMessage "There is no place for the garbage collector!" w
		else changeGen g $ spawnMon mgen xR yR w
	(r, g) = randomR (0, length emptyNeighbors - 1) $ stdgen w
	(xR, yR) = emptyNeighbors !! r


