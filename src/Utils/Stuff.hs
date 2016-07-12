module Utils.Stuff where

import Data.World
import Data.Monster
import Data.Define
import Data.ID
import Utils.Changes
import Utils.Random
import Utils.HealDamage
import Utils.Monsters
import Utils.Items
import Monsters.Parts
import Monsters.AIrepr
import Monsters.Monsters
import Monsters.PartsList
import IO.Colors
import IO.Texts
import MapGen

import System.Random (StdGen, randomR, Random)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A

-- | converts function that needs some random to the function ingoring it 
unrandom :: (a -> a) -> (a, x) -> (a, x)
unrandom f (a, x) = (f a, x)

-- | remove died part and effect of jewelry on them
cleanParts :: Monster -> Monster
cleanParts mon = delEffects $ mon {parts = filter aliveP $ parts mon}
	where delEffects = foldr ((.) . (\ (obj, _) -> effectOff obj 
		$ enchantment obj)) id $ catMaybes
		$ getItem JewelrySlot mon
		<$> filter (not . aliveP) (parts mon)

-- | upgrade body part by increasing 'maxhp' and 'regRate'
upgrade :: Int -> Part -> Part
upgrade n part = part {
	hp = hp part + n,
	maxhp = maxhp part + n,
	regRate = regRate part + 1
}

upgradeParts :: PartKind -> Int -> Monster -> Monster
upgradePartById :: Int -> Int -> Monster -> Monster
upgradeAll :: Int -> Monster -> Monster
-- | upgrade parts with given kind
upgradeParts = doSmthParts upgrade
-- | upgrade parts with given id
upgradePartById = doSmthPartById upgrade
-- | upgrade all parts
upgradeAll = doSmthAll upgrade

-- | add random part to a monster (for potion of mutation)
addRandomPart :: (Monster, StdGen) -> (Monster, StdGen)
addRandomPart (m, g) = (addPart m (toEnum knd) hp' regRate', g3) where
	(knd, g1) = randomR (0, fromEnum Main - 1) g
	(p, g2) = randomR (0.0, 1.0) g1
	hp' = 5 * inverseSquareRandom p
	(regRate', g3) = randomR (1, 4) g2

-- | add part with given characteristic to a monster
addPart :: Monster -> PartKind -> Int -> Int -> Monster
addPart mon knd hp' regRate' = mon {parts = newPart : parts mon} where
	newPart = Part {
		hp = hp',
		maxhp = hp',
		kind = knd,
		idP = newID,
		regRate = regRate',
		objectKeys = M.empty
	}
	newID = (+) 1 $ maximum $ idP <$> parts mon

-- | fire all cells around you (for scroll of fire)
fireAround :: Int -> (Int, Int) -> World -> World
fireAround d pair w = spawnBonfires $ addMessages newMsgs 
	w {units' = newMons, stdgen = g} where
	xNow = xFirst w
	yNow = yFirst w
	(newDmg, g) = randomR pair $ stdgen w
	newMons = mapU (fireDmg g) $ units' w
	isClose ((x, y), _) = abs (x - xNow) <= d && abs (y - yNow) <= d
	fireDmg gen (x, y) mon = 
		if isClose ((x, y), putWE "fireAround")
		then fst $ dmgRandomElem Fire (Just newDmg) mon gen
		else mon
	msg (_,mon) = 
		if name mon == "You"
		then (msgFireYou, red)
		else (name mon ++ msgFire, green)
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

-- | spawn bonfires on cell next to you with some probability
spawnBonfires :: World -> World
spawnBonfires w = foldr spawnBonfire w nears where
	x = xFirst w
	y = yFirst w
	d = [-1, 0, 1]
	nears = [(x + dx, y + dy) | dx <- d, dy <- d]

-- | spawn one bonfire with given coordinates with some probability
spawnBonfire :: (Int, Int) -> World -> World
spawnBonfire (x, y) w = changeTerr x y newTerr w {stdgen = g'} where
	spawnProb :: Float
	spawnProb = 0.2
	(q, g') = randomR (0.0, 1.0) $ stdgen w
	newTerr = if q < spawnProb then Bonfire else terrain $ worldmap w A.! (x, y)

-- | can you untrap this?
isUntrappable :: Cell -> Bool
isUntrappable = (`notElem` [Empty, Water, Bonfire, MagicNatural]). terrain

-- | teleport you to a new safe world
safety :: World -> World
safety w = w {
	units' = (units' w) {list = M.singleton (xFirst w, yFirst w) $ getFirst w},
	message = [(msgSafety, blue)],
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
capture mon = if canWalk mon && not (isPlayer mon)
	then mon {ai = AI $ getPureAI AllyAI}
	else mon

-- | enchant all items in given slot
enchantAll :: Slot -> Int -> Monster -> Monster
enchantAll sl n mon = foldr (maybe id enchantByLetter . 
	M.lookup sl . objectKeys) mon $ parts mon where
	enchantByLetter c mon' = case M.lookup c $ inv mon' of
		Nothing -> mon'
		Just (obj, k) -> updateMon mon' {inv = M.insert c (newObj, k) $ inv mon'} where
			newObj = enchant n obj
			updateMon = if isJewelry obj
				then effectOn newObj (enchantment newObj) . effectOff obj (enchantment obj)
				else id

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
getGarbageCollector = getMonster (getEatAI CollectorAI) partsGrC
	 idGrC ((2,4), 0.4) emptyInv 100 100 10

-- | golem can be created by a Kabbalistic scroll; it will attack all enemies
-- near it
getGolem :: MonsterGen
getGolem = getMonster (getPureAI GolemAI) partsGlm
	idGlm ((2,4), 0.3) emptyInv 100 10000 10

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
