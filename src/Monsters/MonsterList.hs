module Monsters.MonsterList where

import Data.Monster
import Data.Define
import Data.ID
import Items.Stuff
import Monsters.Monsters
import Monsters.AIrepr
import Monsters.PartsList

-- !!! numbers at the end of definitions: slowness, nutrition, strength
getHomunculus, getBeetle, getBat, getHunter, getAccelerator, getTroll,
	getWorm, getFloatingEye, getRedDragon, getWhiteDragon, getGreenDragon,
	getSpider, getSoldier, getUmberHulk, getTree, getBot, getBee :: MonsterGen

-- | homunculus is very stupid and so weak
getHomunculus = getMonster (getHumanoidAI StupidestAI) partsHom
	idHom ((2, 4), 0.4) invHom 100 200 10
-- | beetles are very weak but fast because they have six legs
getBeetle = getMonster (getEatAI StupidAI) partsBtl
	idBtl ((1, 5), 0.1) emptyInv 100 100 5
-- | bats are fat and pretty fast but they can attack you only randomly
getBat = getMonster (getPureAI RandomAI) partsBat
	idBat ((3, 5), 0.2) emptyInv 50 1000 5
-- | hunter is clever and can use bow
getHunter = getMonster (getHunterAI CleverVSAI) partsHun
	idHun ((1, 4), 0.5) hunterInv 60 200 10
-- | accelerator increase its speed every step
getAccelerator = getMonster (AIrepr [AcceleratorAI, EatAI] Nothing StupidAI) partsAcc
	idAcc ((1, 6), 0.2) emptyInv 100 200 8
-- | troll is fat ans stupid; it can transform to a 'rock' instead of death
getTroll = getMonster (AIrepr [TrollAI, EatAI] Nothing StupidAI) partsTrl
	idTrl ((2, 5), 0.4) emptyInv 100 200 20
-- | worm is very long and so fat
getWorm = getMonster (getPureAI WormAI) partsWrm
	idWrm ((5, 8), 0.4) emptyInv 100 1000 30
-- | floating eye can fly and paralyze it, but it is so slow
getFloatingEye = getMonster (getEatAI StupidParalysisAI) partsFlE
	idFlE ((1, 5), 0.2) emptyInv 200 300 5
-- | this dragon will attack you with fire from some distance;
-- this is balaced dragon
getRedDragon = getMonster (getDragonAI Fire 3) partsRDr
	idRDr ((3, 4), 0.2) emptyInv 120 300 15
-- | this dragon will attack you with cold from some distance; 
-- this is powerful dragon
getWhiteDragon = getMonster (getDragonAI Cold 3) partsWDr
	idWDr ((4,5), 0.2) emptyInv 200 300 15
-- | this dragon will attack you with poison from some distance;
-- this is fast dragon
getGreenDragon = getMonster (getDragonAI Poison' 3) partsRDr
	idGDr ((2, 5), 0.2) emptyInv 80 300 15
-- | spider has eight legs and it's poisonous
getSpider = getMonster (getEatAI StupidPoisonAI) partsSpd
	idSpd ((2, 3), 0.1) emptyInv 250 200 5
-- | soldier can wield the weapon and wear the armor;
-- it's almost as clever as you
getSoldier = getMonster (getHumanoidAI CleverSAI) partsSol
	idSol ((1, 10), 0.2) soldierInv 100 300 12
-- | this humanoid can confuse you by gase attack
getUmberHulk = getMonster (getHumanoidAI StupidConfAI) partsUmH
	idUmH ((2, 4), 0.2) emptyInv 100 200 10
-- | Just a tree. Do nothing.
getTree = getMonster (getPureAI NothingAI) partsTre
	idTre ((0, 0), 0) emptyInv 10000 10000 0
-- | clever squeaky light bot
getBot = getMonster (getPureAI CleverVSAI) partsBot
	idBot ((1, 6), 0.1) emptyInv 100 10000 8
-- | very small flying creature
getBee = getMonster (getPureAI StupidestAI) partsBee
	idBee ((1, 6), 0.2) emptyInv 100 200 5
