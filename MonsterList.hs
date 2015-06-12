module MonsterList where

import Data
import Random
import Monsters
import Changes
import Utils4all
import Stuff
import Move
import AI

import System.Random

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
	[getBody 1 $ uniform q 10 30,
	 getHead 1 $ uniform q 10 30,
	 getLeg  1 $ uniform q  5 10,
	 getLeg  1 $ uniform q  5 10,
	 getArm  1 $ uniform q  5 10,
	 getArm  1 $ uniform q  5 10]
	 "Hunter" (dices (1,4) 0.5) 
	 (\p -> [('a', arrow, 10 * inverseSquareRandom p), 
		('b', lAUNCHERS !! uniform p 0 (length lAUNCHERS - 1), 1)]) 60
		
getAccelerator q = getMonster (aiAccelerator stupidAI)
	[getBody 1 $ uniform q 10 20, 
	 getHead 1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  3  7,
	 getLeg  1 $ uniform q  3  7,
	 getArm  1 $ uniform q  2  6,
	 getArm  1 $ uniform q  2  6]
	"Accelerator" (dices (1,6) 0.2) (const []) 150

getIvy q = getMonster aiIvy [getMain 2 $ uniform q 5 15] "Ivy"
	(dices (1,10) 0) (const []) 400

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
	
 
