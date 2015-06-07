module Monsters where

import Data
import Stuff
import Random
import AI
import Utils4all

import System.Random

getMonster :: AIfunc -> [Int -> Part] -> String -> StdDmg -> InvGen -> Int -> MonsterGen
getMonster ai ps name stddmg inv slow x y g = (Monster {
	ai = AI ai,
	parts = zipWith ($) ps [0..],
	x = x,
	y = y,
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
getPart knd hp regVel id = Part {
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

getPlayer :: Int -> Int -> Monster
getPlayer x y = Monster {
	ai = You,
	parts = zipWith ($) 
			[getBody 40 1, 
			 getHead 30 2, 
			 getLeg  20 2, 
			 getLeg  20 2, 
			 getArm  20 2, 
			 getArm  20 2]
			 [0..],
	x = x,
	y = y,
	name = "You",
	stddmg = dices [(1,10)] 0.2,
	inv = [],
	slowness = 100,
	time = 100,
	weapon = ' '
}

getHomunculus = getMonster (aiHumanoid stupidestAI)
	[getBody 20 1, 
	 getHead 10 1,
	 getLeg   5 1,
	 getLeg   5 1,
	 getArm   4 1,
	 getArm   4 1]
	"Homunculus" (dices [(2,4)] 0.4)
	(\p ->
		if p <= 0.1
		then [('a', wandOfStriking 1, 1)]
		else []) 100

getBeetle = getMonster stupidAI
	[getBody 15 1,
	 getHead 10 1,
	 getPaw   7 1,
	 getPaw   7 1,
	 getLeg   5 1,
	 getLeg   5 1,
	 getLeg   5 1,
	 getLeg   5 1]
	 "Beetle" (dices [(1,5)] 0.1) (const []) 100

getBat = getMonster randomAI
	[getBody 30 1, 
	 getHead 20 1,
	 getWing 10 2,
	 getWing 10 2] 
	"Bat" (dices [(3,5)] 0.2) (const []) 50
	
getHunter = getMonster (aiHunter $ aiHumanoid stupidAI) 
	[getBody 30 1, 
	 getHead 20 1,
	 getLeg  10 1,
	 getLeg  10 1, 
	 getArm  10 1,
	 getArm  10 1]
	 "Hunter" (dices [(1,2)] 0.5) 
	 (\p -> [('a', arrow, 10 * inverseSquareRandom p), ('b', longbow, 1)]) 60

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
