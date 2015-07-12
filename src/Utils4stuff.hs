module Utils4stuff where

import Data
import Changes
import Random
import HealDamage
import Parts
import Utils4mon
import Colors
import Texts
import AIrepr
import DataWorld
import DataMonster
import DataDef

import System.Random (StdGen, randomR, Random)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A

unrandom :: (a -> a) -> (a, x) -> (a, x)
unrandom f (a, x) = (f a, x)

cleanParts :: Monster -> Monster
cleanParts mon = delEffects $ changeParts (filter aliveP $ parts mon) mon
	where delEffects = foldr (((.) . (\ (obj, _) -> effectOff obj 
		$ enchantment obj)) . fromJust) id $ filter isJust 
		$ map (flip M.lookup (inv mon) . (\ p -> objectKeys p 
		!! fromEnum JewelrySlot)) $ filter (not . aliveP) $ parts mon

upgrade :: Int -> Part -> Part
upgrade n part = part {
	hp = hp part + n,
	maxhp = maxhp part + n,
	regVel = regVel part + 1
}

upgradeParts, upgradePartById :: Int -> Int -> Monster -> Monster
upgradeAll :: Int -> Monster -> Monster

upgradeParts = doSmthParts upgrade
upgradePartById = doSmthPartById upgrade
upgradeAll = doSmthAll upgrade

addRandomPart :: (Monster, StdGen) -> (Monster, StdGen)
addRandomPart (m, g) = (addPart m knd hp' regVel', g3) where
	(knd, g1) = randomR (0, kINDS) g
	(p, g2) = randomR (0.0, 1.0) g1
	hp' = 5 * inverseSquareRandom p
	(regVel', g3) = randomR (1, 4) g2

addPart :: Monster -> Int -> Int -> Int -> Monster
addPart mon knd hp' regVel' = changeParts (newPart : parts mon) mon where
	newPart = Part {
		hp = hp',
		maxhp = hp',
		kind = knd,
		idP = newID,
		regVel = regVel',
		objectKeys = replicate sLOTS ' '
	}
	newID = (+) 1 $ maximum $ map idP $ parts mon

fireAround :: Int -> (Int, Int) -> World -> World
fireAround d pair w = addMessages newMsgs $ changeGen g $ changeMons newMons w where
	xNow = xFirst w
	yNow = yFirst w
	(newDmg, g) = randomR pair $ stdgen w
	newMons = mapU (fireDmg g) $ units' w
	isClose ((x, y), _) = abs (x - xNow) <= d && abs (y - yNow) <= d
	fireDmg gen (x, y) mon = 
		if isClose ((x, y), lol)
		then fst $ dmgRandomElem Fire (Just newDmg) mon gen
		else mon
	msg (_,mon) = 
		if name mon == "You"
		then (msgFireYou, rED)
		else (name mon ++ msgFire, gREEN)
	newMsgs = map msg $ filter isClose $ M.toList $ units w

stupidity :: Monster -> Monster
stupidity mon = mon {ai = newAI} where
	newAI = case ai mon of
		You -> You
		old@(AI _) -> 
			if canWalk mon
			then AI $ getPureAI StupidestAI
			else old
	
isUntrappable :: Terrain -> Bool
isUntrappable = (/=) Empty
	
safety :: World -> World
safety w = w {
	units' = (units' w) {list = M.singleton (xFirst w, yFirst w) $ getFirst w},
	message = [(msgSafety, bLUE)],
	items = [],
	action = Move,
	wave = wave w + 1,
	chars = S.empty,
	worldmap = A.listArray ((0,0), (maxX,maxY)) $ repeat Empty,
	stepsBeforeWave = 2
} 

speed :: Int -> Monster -> Monster
speed x m = m {slowness = slowness m - x}

radiation :: Int -> Monster -> Monster
radiation sp m = m {parts = map (\p -> p {regVel = regVel p - sp}) $ parts m}

capture :: Monster -> Monster
capture mon = if canWalk mon then mon {ai = You} else mon

enchantAll :: Slot -> Int -> Monster -> Monster
enchantAll sl n mon = foldr (enchantByLetter . 
	(\ p -> objectKeys p !! fromEnum sl)) mon $ parts mon where
	enchantByLetter c mon' = case M.lookup c $ inv mon' of
		Nothing -> mon'
		Just (obj, k) -> mon' {inv = M.insert c (enchant n obj, k) $ inv mon'}

actWithFirst :: ((Monster, StdGen) -> (Monster, StdGen)) -> World -> World
actWithFirst f w = changeGen newGen $ changeMon newMon w where
	(newMon, newGen) = f (getFirst w, stdgen w)

addRandom :: Random a => (a, a) -> (a -> b -> c) -> (b, StdGen) -> (c, StdGen)
addRandom pair f (o, g) = (f value o, g') where
	(value, g') = randomR pair g
