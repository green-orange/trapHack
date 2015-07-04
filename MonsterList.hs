module MonsterList where

import Data
import Random
import Monsters
import Stuff
import Move
import AI
import Parts
--import Forgotten

import qualified Data.Map as M
--import System.Random (StdGen)

getHomunculus, getBeetle, getBat, getHunter, getAccelerator, getTroll,
	getWorm, getFloatingEye, getRedDragon, getWhiteDragon, getGreenDragon,
	{-getForgottenBeast,-} getSpider, getSoldier, getUmberHulk :: MonsterGen

getHomunculus = getMonster (humanoidAI stupidestAI)
	[(getBody 1, (10, 30)), 
	 (getHead 1, (8, 12)),
	 (getLeg 1, (3, 7)),
	 (getLeg 1, (3, 7)),
	 (getArm 1, (2, 6)),
	 (getArm 1, (2, 6))]
	"Homunculus" (dices (2,4) 0.4) (const M.empty) 100
	{-(\p ->
		if p <= 0.1
		then M.singleton 'a' (wandForHom q 1, 1)
		else M.empty) 100-}

wandForHom :: Float -> (Int -> Object)
wandForHom = flip uniformFromList [wandOfPoison, wandOfSlowing, 
	wandOfSpeed, wandOfStriking, wandOfStun]

getBeetle = getMonster stupidAI
	[(getBody 1, (10, 20)),
	 (getHead 1, ( 5, 15)),
	 (getPaw  1, ( 5,  9)),
	 (getPaw  1, ( 5,  9)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8))]
	"Beetle" (dices (1,5) 0.1) (const M.empty) 100

getBat = getMonster randomAI
	[(getBody 1, (10, 50)), 
	 (getHead 1, (5, 35)),
	 (getWing 2, (5, 15)),
	 (getWing 2, (5, 15))]
	"Bat" (dices (3,5) 0.2) (const M.empty) 50
	
getHunter = getMonster (hunterAI $ humanoidAI stupidAI) 
	[(getBody 1, (10, 30)),
	 (getHead 1, (10, 30)),
	 (getLeg 1, (5, 10)),
	 (getLeg 1, (5, 10)),
	 (getArm 1, (5, 10)),
	 (getArm 1, (5, 10))]
	"Hunter" (dices (1,4) 0.5) 
	{-(\p -> M.insert 'a' (arrow, 10 * inverseSquareRandom p) $
		M.singleton 'b' (lAUNCHERS !! uniform p 0 (length lAUNCHERS - 1), 1)) 60-}
	(const M.empty) 60
		
getAccelerator = getMonster (acceleratorAI stupidAI)
	[(getBody 1, (10, 20)), 
	 (getHead 1, (8, 12)),
	 (getLeg 1, (3, 7)),
	 (getLeg 1, (3 ,7)),
	 (getArm 1, (2, 6)),
	 (getArm 1, (2, 6))]
	"Accelerator" (dices (1,6) 0.2) (const M.empty) 150
	
getTroll = getMonster (trollAI stupidAI)
	[(getBody 2, (10, 30)),
	 (getHead 2, (10, 20)),
	 (getLeg 3, (8, 12)),
	 (getLeg 3, (8, 12)),
	 (getArm 3, (8, 12)),
	 (getArm 3, (8, 12))]
	"Troll" (dices (2,5) 0.4) (const M.empty) 100
	 
getWorm = getMonster wormAI
	[(getMain 1, (200, 500))]
	"Worm" (dices (5,8) 0.4) (const M.empty) 100
	
getFloatingEye = getMonster stupidParalysisAI
	[(getMain 2, (10, 40)),
	 (getWing 1, (5, 10)),
	 (getWing 1, (5, 10))]
	"Floating eye" (dices (1,5) 0.2) (const M.empty) 200
	
getRedDragon = getMonster (attackIfClose Fire 3 $ stupidAI)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (5, 15)),
	 (getLeg 1, (5, 15)),
	 (getWing 3, (5, 15)),
	 (getWing 3, (5, 15))]
	"Red dragon" (dices (3,4) 0.2) (const M.empty) 120

getWhiteDragon = getMonster (attackIfClose Cold 3 $ stupidAI)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (5, 15)),
	 (getLeg 1, (5, 15)),
	 (getWing 3, (10, 20)),
	 (getWing 3, (10, 20))]
	"White dragon" (dices (4,5) 0.2) (const M.empty) 200
	
getGreenDragon = getMonster (attackIfClose Poison' 3 $ stupidAI)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (10, 20)),
	 (getLeg 1, (10, 20)),
	 (getWing 3, (10, 30)),
	 (getWing 3, (10, 30))]
	"Green dragon" (dices (2,5) 0.2) (const M.empty) 80
{-	
getForgottenBeast = getMonster (forgottenAI q) (forgottenParts q)
	"Forgotten beast" (forgottenDmg q) forgottenInv (forgottenSlowness q)
-}
getSpider = getMonster stupidPoisonAI
	[(getBody 1, (10, 20)),
	 (getHead 1, (5, 15)),
	 (getPaw 1, (5, 8)),
	 (getPaw 1, (5, 8)),
	 (getLeg 1, (2, 5)),
	 (getLeg 1, (2, 5)),
	 (getLeg 1, (2, 5)),
	 (getLeg 1, (2, 5)),
	 (getLeg 1, (2, 5)),
	 (getLeg 1, (2, 5))]
	"Spider" (dices (2,3) 0.1) (const M.empty) 250
	
getSoldier = getMonster (humanoidAI stupidAI)
	[(getBody 2, (10, 30)),
	 (getHead 2, (10, 20)),
	 (getLeg 3, (8, 12)),
	 (getLeg 3, (8, 12)),
	 (getArm 3, (8, 12)),
	 (getArm 3, (8, 12))]
	"Soldier" (dices (1,10) 0.2) {-soldierInv-} (const M.empty) 100

soldierInv :: InvGen
soldierInv q = M.fromList $ zip alphabet $ flip zip [1,1..] 
	$ uniformFromList q wEAPONS :
	uniformFromList (frac $ 2 * q + c) aRMOR : 
	uniformFromList (frac $ 3 * q + 2 * c) aRMOR : [] where
	c = 0.28989850379988

getUmberHulk = getMonster (humanoidAI stupidConfAI)
	[(getBody 2, (10, 20)),
	 (getHead 2, (10, 15)),
	 (getLeg 3, (5, 10)),
	 (getLeg 3, (5, 10)),
	 (getArm 3, (5, 10)),
	 (getArm 3, (5, 10))]
	"Umber hulk" (dices (2,4) 0.2) (const M.empty) 100
