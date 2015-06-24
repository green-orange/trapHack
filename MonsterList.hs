module MonsterList where

import Data
import Random
import Monsters
import Stuff
import Move
import AI
import Parts
import Forgotten

import qualified Data.Map as M

getHomunculus, getBeetle, getBat, getHunter, getAccelerator, getTroll,
	getWorm, getFloatingEye, getRedDragon, getWhiteDragon, getGreenDragon,
	getForgottenBeast, getSpider, getSoldier :: Float -> MonsterGen

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
	"Troll" (dices (2,5) 0.4) (const M.empty) 100
	 
getWorm q = getMonster wormAI
	[getMain 1 $ uniform q 200 500]
	"Worm" (dices (5,8) 0.4) (const M.empty) 100
	
getFloatingEye q = getMonster stupidParalysisAI
	[getMain 2 $ uniform q 10 40,
	 getWing 1 $ uniform q  5 10,
	 getWing 1 $ uniform q  5 10]
	"Floating eye" (dices (1,5) 0.2) (const M.empty) 200
	
getRedDragon q = getMonster (attackIfClose Fire 3 $ stupidAI)
	[getBody 2 $ uniform q 10 40,
	 getHead 2 $ uniform q 10 30,
	 getLeg  1 $ uniform q  5 15,
	 getLeg  1 $ uniform q  5 15,
	 getWing 3 $ uniform q  5 15,
	 getWing 3 $ uniform q  5 15]
	"Red dragon" (dices (3,4) 0.2) (const M.empty) 120

getWhiteDragon q = getMonster (attackIfClose Cold 3 $ stupidAI)
	[getBody 2 $ uniform q 10 40,
	 getHead 2 $ uniform q 10 30,
	 getLeg  1 $ uniform q  5 15,
	 getLeg  1 $ uniform q  5 15,
	 getWing 3 $ uniform q 10 20,
	 getWing 3 $ uniform q 10 20]
	"White dragon" (dices (4,5) 0.2) (const M.empty) 200
	
getGreenDragon q = getMonster (attackIfClose Poison' 3 $ stupidAI)
	[getBody 2 $ uniform q 10 40,
	 getHead 2 $ uniform q 10 30,
	 getLeg  1 $ uniform q 10 20,
	 getLeg  1 $ uniform q 10 20,
	 getWing 3 $ uniform q 10 30,
	 getWing 3 $ uniform q 10 30]
	"Green dragon" (dices (2,5) 0.2) (const M.empty) 80
	
getForgottenBeast q = getMonster (forgottenAI q) (forgottenParts q)
	"Forgotten beast" (forgottenDmg q) forgottenInv (forgottenSlowness q)

getSpider q = getMonster stupidPoisonAI
	[getBody 1 $ uniform q 10 20,
	 getHead 1 $ uniform q  5 15,
	 getPaw  1 $ uniform q  5  8,
	 getPaw  1 $ uniform q  5  8,
	 getLeg  1 $ uniform q  2  5,
	 getLeg  1 $ uniform q  2  5,
	 getLeg  1 $ uniform q  2  5,
	 getLeg  1 $ uniform q  2  5,
	 getLeg  1 $ uniform q  2  5,
	 getLeg  1 $ uniform q  2  5]
	"Spider" (dices (2,3) 0.1) (const M.empty) 250
	
getSoldier q = getMonster (humanoidAI stupidAI)
	[getBody 2 $ uniform q 10 30,
	 getHead 2 $ uniform q 10 20,
	 getLeg  3 $ uniform q  8 12,
	 getLeg  3 $ uniform q  8 12,
	 getArm  3 $ uniform q  8 12,
	 getArm  3 $ uniform q  8 12]
	"Soldier" (dices (1,10) 0.2) soldierInv 100

soldierInv :: InvGen
soldierInv q = M.fromList $ zip alphabet $ flip zip [1,1..] 
	$ uniformFromList q wEAPONS :
	uniformFromList (frac $ 2 * q + c) aRMOR : 
	uniformFromList (frac $ 3 * q + 2 * c) aRMOR : [] where
	c = 0.28989850379988
