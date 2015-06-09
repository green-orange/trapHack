module Monsters where

import Data
import Stuff
import Random
import AI
import Utils4all
import Changes
import Move

import System.Random

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

getHomunculus q = getMonster (aiHumanoid stupidestAI)
	[getBody 1 $ uniform q 10 30, 
	 getHead 1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  3  7,
	 getLeg  1 $ uniform q  3  7,
	 getArm  1 $ uniform q  2  6,
	 getArm  1 $ uniform q  2  6]
	"Homunculus" (dices (2,4) 0.4)
	(\p ->
		if p <= 0.1
		then [('a', wandOfStriking 1, 1)]
		else []) 100

getBeetle q = getMonster stupidAI
	[getBody 1 $ uniform q 10 20,
	 getHead 1 $ uniform q  5 15,
	 getPaw  1 $ uniform q  5  9,
	 getPaw  1 $ uniform q  5  9,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8]
	 "Beetle" (dices (1,5) 0.1) (const []) 100

getBat q = getMonster randomAI
	[getBody 1 $ uniform q 10 50, 
	 getHead 1 $ uniform q  5 35,
	 getWing 2 $ uniform q  5 15,
	 getWing 2 $ uniform q  5 15] 
	"Bat" (dices (3,5) 0.2) (const []) 50
	
getHunter q = getMonster (aiHunter $ aiHumanoid stupidAI) 
	[getBody 1 $ uniform q 20 40, 
	 getHead 1 $ uniform q 10 30,
	 getLeg  1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  8 12,
	 getArm  1 $ uniform q  8 12,
	 getArm  1 $ uniform q  8 12]
	 "Hunter" (dices (1,4) 0.5) 
	 (\p -> [('a', arrow, 10 * inverseSquareRandom p), 
		('b', lAUNCHERS !! uniform p 0 (length lAUNCHERS - 1), 1)]) 60

aiIvy :: AIfunc
aiIvy world xPlayer yPlayer = 
	if abs dx <= 1 && abs dy <= 1
	then moveFirst world dx dy
	else if isEmpty world (xNow + dx') (yNow + dy')
	then spawnMon (getIvy q) (xNow + dx') (yNow + dy') $ changeGen g''' world
	else world where
		(xNow, yNow, _) = head $ units world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
		(q, g''') = randomR (0.0, 1.0) g''

getIvy q = getMonster aiIvy [getMain 2 $ uniform q 5 15] "Ivy"
	(dices (1,5) 0) (const []) 150

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
