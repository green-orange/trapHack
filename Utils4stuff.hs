module Utils4stuff where

import Data
import Changes
import Random
import Move (stupidestAI)
import HealDamage
import Parts
import Utils4mon

import System.Random (StdGen, randomR)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import qualified Data.Map as M

unrandom :: (a -> a) -> (a, x) -> (a, x)
unrandom f (a, x) = (f a, x)

cleanParts :: Monster -> Monster
cleanParts mon = delEffects $ changeParts (filter aliveP $ parts mon) mon
	where delEffects = foldr (.) id $ map (\(obj,_) -> effectOff obj 
		$ enchantment obj) $ map fromJust $ filter isJust 
		$ map (flip M.lookup $ inv mon) $ map (\p -> objectKeys p 
		!! fromEnum JewelrySlot) $ filter (not . aliveP) $ parts mon

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
		then fst $ dmgRandom (Just newDmg) mon gen
		else mon
	msg (_,mon) = 
		if name mon == "You"
		then ("You are in fire!", rED)
		else (name mon ++ " is in fire!", gREEN)
	newMsgs = map msg $ filter isClose $ M.toList $ units w
	
stupidity :: Monster -> Monster
stupidity mon = mon {ai = newAI} where
	newAI = case ai mon of
		You -> You
		old@(AI _) -> 
			if canWalk mon
			then AI stupidestAI
			else old
	
isUntrappable :: Terrain -> Bool
isUntrappable = (/=) eMPTY
	
safety :: World -> World
safety w = w {
	units' = (units' w) {list = M.singleton (xFirst w, yFirst w) $ getFirst w},
	message = [("You suddenly find yourself in a new world!", bLUE)],
	items = [],
	action = ' ',
	wave = wave w + 1,
	chars = S.empty,
	worldmap = map (map $ const eMPTY) $ worldmap w,
	stepsBeforeWave = 2
} 

speed :: Int -> Monster -> Monster
speed x m = m {slowness = max 10 $ slowness m - x}

radiation :: Int -> Monster -> Monster
radiation sp m = m {parts = map (\p -> p {regVel = regVel p - sp}) $ parts m}

capture :: Monster -> Monster
capture mon = if canWalk mon then mon {ai = You} else mon

enchantAll :: Slot -> Int -> Monster -> Monster
enchantAll sl n mon = foldr ($) mon $ map (enchantByLetter 
	. (\p -> objectKeys p !! fromEnum sl)) $ parts mon where
	enchantByLetter c mon' = case M.lookup c $ inv mon' of
		Nothing -> mon'
		Just (obj, k) -> mon' {inv = M.insert c (enchant n obj, k) $ inv mon'}
