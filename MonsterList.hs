module MonsterList where

import Data
import Random
import Monsters
import Changes
import Utils4all
import Utils4mon
import Stuff
import Move
import AI
import Parts

import System.Random
import qualified Data.Map as M

getHomunculus q = getMonster (humanoidAI stupidestAI)
	[getBody 1 $ uniform q 10 30, 
	 getHead 1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  3  7,
	 getLeg  1 $ uniform q  3  7,
	 getArm  1 $ uniform q  2  6,
	 getArm  1 $ uniform q  2  6]
	"Homunculus" (dices (2,4) 0.4)
	(\p ->
		if p <= 0.1
		then M.singleton 'a' (wandOfStriking 1, 1)
		else M.empty) 100

getBeetle q = getMonster stupidAI
	[getBody 1 $ uniform q 10 20,
	 getHead 1 $ uniform q  5 15,
	 getPaw  1 $ uniform q  5  9,
	 getPaw  1 $ uniform q  5  9,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8,
	 getLeg  1 $ uniform q  2  8]
	 "Beetle" (dices (1,5) 0.1) (const M.empty) 100

getBat q = getMonster randomAI
	[getBody 1 $ uniform q 10 50, 
	 getHead 1 $ uniform q  5 35,
	 getWing 2 $ uniform q  5 15,
	 getWing 2 $ uniform q  5 15] 
	"Bat" (dices (3,5) 0.2) (const M.empty) 50
	
getHunter q = getMonster (hunterAI $ humanoidAI stupidAI) 
	[getBody 1 $ uniform q 10 30,
	 getHead 1 $ uniform q 10 30,
	 getLeg  1 $ uniform q  5 10,
	 getLeg  1 $ uniform q  5 10,
	 getArm  1 $ uniform q  5 10,
	 getArm  1 $ uniform q  5 10]
	 "Hunter" (dices (1,4) 0.5) 
	 (\p -> M.insert 'a' (arrow, 10 * inverseSquareRandom p) $
		M.singleton 'b' (lAUNCHERS !! uniform p 0 (length lAUNCHERS - 1), 1)) 60
		
getAccelerator q = getMonster (acceleratorAI stupidAI)
	[getBody 1 $ uniform q 10 20, 
	 getHead 1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  3  7,
	 getLeg  1 $ uniform q  3  7,
	 getArm  1 $ uniform q  2  6,
	 getArm  1 $ uniform q  2  6]
	"Accelerator" (dices (1,6) 0.2) (const M.empty) 150
	
getTroll q = getMonster (trollAI stupidAI)
	[getBody 2 $ uniform q 10 30,
	 getHead 2 $ uniform q 10 20,
	 getLeg  3 $ uniform q  8 12,
	 getLeg  3 $ uniform q  8 12,
	 getArm  3 $ uniform q  8 12,
	 getArm  3 $ uniform q  8 12]
	 "Troll" (dices (2,5) 0.2) (const M.empty) 100

getIvy q = getMonster ivyAI [getMain 2 $ uniform q 5 15] "Ivy"
	(dices (2,10) 0) (const M.empty) 600

ivyAI :: AIfunc
ivyAI world xPlayer yPlayer = 
	if abs dx <= 1 && abs dy <= 1
	then moveFirst world dx dy
	else if isEmpty world (xNow + dx') (yNow + dy')
	then spawnMon (getIvy q) (xNow + dx') (yNow + dy') $ changeGen g''' world
	else killFirst world where
		(xNow, yNow, _) = head $ units world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
		(q, g''') = randomR (0.0, 1.0) g''
	
 
