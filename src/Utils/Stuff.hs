module Utils.Stuff where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Random
import Utils.HealDamage
import Utils.Monsters
import Monsters.Parts
import Monsters.AIrepr
import Monsters.Monsters
import IO.Colors
import IO.Texts
import MapGen

import System.Random (StdGen, randomR, Random)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map as M

-- | converts function that needs some random to the function ingoring it 
unrandom :: (a -> a) -> (a, x) -> (a, x)
unrandom f (a, x) = (f a, x)

-- | remove died part and effect of jewelry on them
cleanParts :: Monster -> Monster
cleanParts mon = delEffects $ mon {parts = filter aliveP $ parts mon}
	where delEffects = foldr (((.) . (\ (obj, _) -> effectOff obj 
		$ enchantment obj)) . fromJust) id $ filter isJust 
		$ (flip M.lookup (inv mon) . (\ p -> objectKeys p 
		!! fromEnum JewelrySlot)) <$> filter (not . aliveP) (parts mon)

-- | upgrade body part by increasing 'maxhp' and 'regRate'
upgrade :: Int -> Part -> Part
upgrade n part = part {
	hp = hp part + n,
	maxhp = maxhp part + n,
	regRate = regRate part + 1
}

upgradeParts, upgradePartById :: Int -> Int -> Monster -> Monster
upgradeAll :: Int -> Monster -> Monster
-- | upgrade parts with given kind
upgradeParts = doSmthParts upgrade
-- | upgrade parts with given id
upgradePartById = doSmthPartById upgrade
-- | upgrade all parts
upgradeAll = doSmthAll upgrade

-- | add random part to a monster (for potion of mutation)
addRandomPart :: (Monster, StdGen) -> (Monster, StdGen)
addRandomPart (m, g) = (addPart m knd hp' regRate', g3) where
	(knd, g1) = randomR (0, kINDS) g
	(p, g2) = randomR (0.0, 1.0) g1
	hp' = 5 * inverseSquareRandom p
	(regRate', g3) = randomR (1, 4) g2

-- | add part with given characteristic to a monster
addPart :: Monster -> Int -> Int -> Int -> Monster
addPart mon knd hp' regRate' = mon {parts = newPart : parts mon} where
	newPart = Part {
		hp = hp',
		maxhp = hp',
		kind = knd,
		idP = newID,
		regRate = regRate',
		objectKeys = replicate sLOTS ' '
	}
	newID = (+) 1 $ maximum $ idP <$> parts mon

-- | fire all cells around you (for scroll of fire)
fireAround :: Int -> (Int, Int) -> World -> World
fireAround d pair w = addMessages newMsgs w {units' = newMons, stdgen = g} where
	xNow = xFirst w
	yNow = yFirst w
	(newDmg, g) = randomR pair $ stdgen w
	newMons = mapU (fireDmg g) $ units' w
	isClose ((x, y), _) = abs (x - xNow) <= d && abs (y - yNow) <= d
	fireDmg gen (x, y) mon = 
		if isClose ((x, y), undefined)
		then fst $ dmgRandomElem Fire (Just newDmg) mon gen
		else mon
	msg (_,mon) = 
		if name mon == "You"
		then (msgFireYou, rED)
		else (name mon ++ msgFire, gREEN)
	newMsgs = msg <$> filter isClose (M.toList $ units w)

-- | change AI to 'StupidestAI' if monster is active and isn't player
stupidity :: Monster -> Monster
stupidity mon = mon {ai = newAI} where
	newAI = case ai mon of
		You -> You
		old@(AI _) -> 
			if canWalk mon
			then AI $ getPureAI StupidestAI
			else old

-- | can you untrap this?
isUntrappable :: Cell -> Bool
isUntrappable = (`notElem` [Empty, Water]). terrain

-- | teleport you to a new safe world
safety :: World -> World
safety w = w {
	units' = (units' w) {list = M.singleton (xFirst w, yFirst w) $ getFirst w},
	message = [(msgSafety, bLUE)],
	items = [],
	action = Move,
	wave = wave w + 1,
	chars = S.empty,
	stdgen = newGen,
	worldmap = newMap
} where (newMap, newGen) = runMap (mapType w) $ stdgen w

-- | speed up the monster
speed :: Int -> Monster -> Monster
speed x m = m {slowness = slowness m - x}

-- | increase radiation for all parts of the monster
radiation :: Int -> Monster -> Monster
radiation sp m = m {parts = (\p -> p {regRate = regRate p - sp}) <$> parts m}

-- | change AI to 'You' if monster is active
capture :: Monster -> Monster
capture mon = if canWalk mon then mon {ai = You} else mon

-- | enchant all items in given slot
enchantAll :: Slot -> Int -> Monster -> Monster
enchantAll sl n mon = foldr (enchantByLetter . 
	(\ p -> objectKeys p !! fromEnum sl)) mon $ parts mon where
	enchantByLetter c mon' = case M.lookup c $ inv mon' of
		Nothing -> mon'
		Just (obj, k) -> mon' {inv = M.insert c (enchant n obj, k) $ inv mon'}

-- | do something only with first monster
actWithFirst :: ((Monster, StdGen) -> (Monster, StdGen)) -> World -> World
actWithFirst f w = changeMon newMon w {stdgen = newGen} where
	(newMon, newGen) = f (getFirst w, stdgen w)

-- | convert function without random to randomized function
addRandom :: Random a => (a, a) -> (a -> b -> c) -> (b, StdGen) -> (c, StdGen)
addRandom pair f (o, g) = (f value o, g') where
	(value, g') = randomR pair g

-- | garbage collector can help you in picking all items from the ground;
-- it can be creating by a scroll of collecting
getGarbageCollector :: MonsterGen		  
getGarbageCollector = getMonster (getEatAI CollectorAI)
	[(getBody 1, (20, 40)), 
	 (getHead 1, (10, 30)),
	 (getLeg  1, ( 8, 12)),
	 (getLeg  1, ( 8, 12)),
	 (getArm  1, ( 8, 12)),
	 (getArm  1, ( 8, 12))]
	 17 ((2,4), 0.4) emptyInv 100 100

-- | golem can be created by a Kabbalistic scroll; it will attack all enemies
-- near it
getGolem :: MonsterGen
getGolem = getMonster (getPureAI GolemAI)
	[(getBody 1, (10, 30)), 
	 (getHead 1, ( 8, 12)),
	 (getLeg  1, ( 3,  7)),
	 (getLeg  1, ( 3,  7)),
	 (getArm  1, ( 2,  6)),
	 (getArm  1, ( 2,  6))]
	18 ((2,4), 0.3) emptyInv 100 10000

-- | spawn a golem next to you
spawnGolem :: Int -> Int -> World -> World
spawnGolem x y w = 
	if isEmpty w x y && correct
	then spawnMon getGolem x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		correct = any filterfun $ items w
		newItems = filter (not . filterfun) $ items w

-- | spawn golem from all items around you
spawnGolemsAround :: World -> World
spawnGolemsAround = fooAround spawnGolem

-- | increase monster 'Nutrition'
addNutr :: Int -> Monster -> Monster
addNutr n mon = changeTemp Nutrition 
	((+n) <$> temp mon !! fromEnum Nutrition) mon
