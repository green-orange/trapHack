module Monsters.MonsterList where

import Data.Const
import Data.Monster
import Data.Define
import Utils.Random
import Items.Stuff
import Monsters.Monsters
import Monsters.Parts
import Monsters.AIrepr

import qualified Data.Map as M
import System.Random (randomR)

getHomunculus, getBeetle, getBat, getHunter, getAccelerator, getTroll,
	getWorm, getFloatingEye, getRedDragon, getWhiteDragon, getGreenDragon,
	getSpider, getSoldier, getUmberHulk, getTree, getBot :: MonsterGen

getHomunculus = getMonster (getHumanoidAI StupidestAI)
	[(getBody 1, (10, 30)), 
	 (getHead 1, (8, 12)),
	 (getLeg 1, (3, 7)),
	 (getLeg 1, (3, 7)),
	 (getArm 1, (2, 6)),
	 (getArm 1, (2, 6))]
	1 ((2,4), 0.4)
	(\g -> let
		p, q :: Float
		(p, g') = randomR (0.0, 1.0) g 
		(q, g'') = randomR (0.0, 1.0) g' in
		(if p <= 0.1
		then M.singleton 'a' (wandForHom q 1, 1)
		else M.empty, g'')) 100 200

wandForHom :: Float -> Int -> Object
wandForHom = flip uniformFromList [wandOfPoison, wandOfSlowing, 
	wandOfSpeed, wandOfStriking, wandOfStun]

getBeetle = getMonster (getEatAI StupidAI)
	[(getBody 1, (10, 20)),
	 (getHead 1, ( 5, 15)),
	 (getPaw  1, ( 5,  9)),
	 (getPaw  1, ( 5,  9)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8)),
	 (getLeg 1, (2, 8))]
	2 ((1,5), 0.1) emptyInv 100 100

getBat = getMonster (getPureAI RandomAI)
	[(getBody 1, (10, 50)), 
	 (getHead 1, (5, 35)),
	 (getWing 2, (5, 15)),
	 (getWing 2, (5, 15))]
	3 ((3,5), 0.2) emptyInv 50 1000
	
getHunter = getMonster (getHunterAI CleverVSAI)
	[(getBody 1, (10, 30)),
	 (getHead 1, (10, 30)),
	 (getLeg 1, (5, 10)),
	 (getLeg 1, (5, 10)),
	 (getArm 1, (5, 10)),
	 (getArm 1, (5, 10))]
	4 ((1,4), 0.5) hunterInv 60 200 

hunterInv :: InvGen
hunterInv g = (M.fromList $ zip alphabet $ addRation
	[(arrow, 10 * inverseSquareRandom p)
	,(lAUNCHERS !! uniform q 0 (length lAUNCHERS - 1), 1)
	], g3) where
		(p, g1) = randomR (0.0, 1.0) g
		(q, g2) = randomR (0.0, 1.0) g1
		(n, g3) = randomR (0, 2) g2
		addRation = if n == 0 then id else (:) (foodRation, n)
	
getAccelerator = getMonster (AIrepr [AcceleratorAI, EatAI] Nothing StupidAI)
	[(getBody 1, (10, 20)), 
	 (getHead 1, (8, 12)),
	 (getLeg 1, (3, 7)),
	 (getLeg 1, (3 ,7)),
	 (getArm 1, (2, 6)),
	 (getArm 1, (2, 6))]
	5 ((1,6), 0.2) emptyInv 150 200
	
getTroll = getMonster (AIrepr [TrollAI, EatAI] Nothing StupidAI)
	[(getBody 2, (10, 30)),
	 (getHead 2, (10, 20)),
	 (getLeg 3, (8, 12)),
	 (getLeg 3, (8, 12)),
	 (getArm 3, (8, 12)),
	 (getArm 3, (8, 12))]
	6 ((2,5), 0.4) emptyInv 100 200
	 
getWorm = getMonster (getPureAI WormAI)
	[(getMain 1, (200, 500))]
	7 ((5,8), 0.4) emptyInv 100 1000
	
getFloatingEye = getMonster (getEatAI StupidParalysisAI)
	[(getMain 2, (10, 40)),
	 (getWing 1, (5, 10)),
	 (getWing 1, (5, 10))]
	8 ((1,5), 0.2) emptyInv 200 300
	
getRedDragon = getMonster (getDragonAI Fire 3)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (5, 15)),
	 (getLeg 1, (5, 15)),
	 (getWing 3, (5, 15)),
	 (getWing 3, (5, 15))]
	9 ((3,4), 0.2) emptyInv 120 300

getWhiteDragon = getMonster (getDragonAI Cold 3)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (5, 15)),
	 (getLeg 1, (5, 15)),
	 (getWing 3, (10, 20)),
	 (getWing 3, (10, 20))]
	10 ((4,5), 0.2) emptyInv 200 300
	
getGreenDragon = getMonster (getDragonAI Poison' 3)
	[(getBody 2, (10, 40)),
	 (getHead 2, (10, 30)),
	 (getLeg 1, (10, 20)),
	 (getLeg 1, (10, 20)),
	 (getWing 3, (10, 30)),
	 (getWing 3, (10, 30))]
	11 ((2,5), 0.2) emptyInv 80 300

getSpider = getMonster (getEatAI StupidPoisonAI)
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
	12 ((2,3), 0.1) emptyInv 250 200
	
getSoldier = getMonster (getHumanoidAI CleverSAI)
	[(getBody 2, (10, 30)),
	 (getHead 2, (10, 20)),
	 (getLeg 3, (8, 12)),
	 (getLeg 3, (8, 12)),
	 (getArm 3, (8, 12)),
	 (getArm 3, (8, 12))]
	13 ((1,10), 0.2) soldierInv 100 300

soldierInv :: InvGen
soldierInv g = (M.fromList $ zip alphabet $ addRation $ zip
	[uniformFromList x1 wEAPONS,
	uniformFromList x2 aRMOR,
	uniformFromList x3 aRMOR] [1,1..], g4) where
		(x1, g1) = randomR (0.0, 1.0) g
		(x2, g2) = randomR (0.0, 1.0) g1
		(x3, g3) = randomR (0.0, 1.0) g2
		(n,  g4) = randomR (0, 2) g3
		addRation = if n == 0 then id else (:) (foodRation, n)

getUmberHulk = getMonster (getHumanoidAI StupidConfAI)
	[(getBody 2, (10, 20)),
	 (getHead 2, (10, 15)),
	 (getLeg 3, (5, 10)),
	 (getLeg 3, (5, 10)),
	 (getArm 3, (5, 10)),
	 (getArm 3, (5, 10))]
	14 ((2,4), 0.2) emptyInv 100 200

getTree = getMonster (getPureAI NothingAI) [(getMain 1, (50, 100))] 22
	((0, 0), 0) emptyInv 10000 10000

getBot = getMonster (getPureAI CleverVSAI)
	[(getBody 1, (10, 15)),
	 (getHead 1, (8, 12)),
	 (getLeg 1, (1, 6)),
	 (getLeg 1, (1, 6)),
	 (getArm 1, (1, 6)),
	 (getArm 1, (1, 6))]
	23 ((1, 4), 0.1) emptyInv 100 10000
