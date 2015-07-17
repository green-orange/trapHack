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

import System.Random (StdGen, randomR, Random)
import Data.Maybe (isJust, fromJust)
import Control.Applicative ((<$>))
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
	regRate = regRate part + 1
}

upgradeParts, upgradePartById :: Int -> Int -> Monster -> Monster
upgradeAll :: Int -> Monster -> Monster

upgradeParts = doSmthParts upgrade
upgradePartById = doSmthPartById upgrade
upgradeAll = doSmthAll upgrade

addRandomPart :: (Monster, StdGen) -> (Monster, StdGen)
addRandomPart (m, g) = (addPart m knd hp' regRate', g3) where
	(knd, g1) = randomR (0, kINDS) g
	(p, g2) = randomR (0.0, 1.0) g1
	hp' = 5 * inverseSquareRandom p
	(regRate', g3) = randomR (1, 4) g2

addPart :: Monster -> Int -> Int -> Int -> Monster
addPart mon knd hp' regRate' = changeParts (newPart : parts mon) mon where
	newPart = Part {
		hp = hp',
		maxhp = hp',
		kind = knd,
		idP = newID,
		regRate = regRate',
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
	
isUntrappable :: Cell -> Bool
isUntrappable = (Empty /=) . terrain

getSinFunc :: Float -> Float -> StdGen -> (Int -> Int -> Float, StdGen)
getSinFunc maxA maxB g = (sinf, g3) where
	(a, g1) = randomR (0.0, maxA) g
	(b, g2) = randomR (0.0, maxB) g1
	(c, g3) = randomR (0.0, 2 * pi) g2
	sinf x y = sin $ a * fromIntegral x + b * fromIntegral y + c

addSinFunc :: Float -> Float -> (Int -> Int -> Float, StdGen) 
	-> (Int -> Int -> Float, StdGen)
addSinFunc maxA maxB (f, gen) = (\x y -> f x y + f' x y, gen') where
	(f', gen') = getSinFunc maxA maxB gen

cntSin :: Int
cntSin = 30

getMap :: StdGen -> (A.Array (Int, Int) Cell, StdGen)
getMap gen = (rez, newGen) where
	cntSinF :: Float
	cntSinF = fromIntegral cntSin
	maxX' = 40 * pi / fromIntegral maxX
	maxY' = 40 * pi / fromIntegral maxY
	(sinf, newGen) = (foldr (.) id $ replicate cntSin $ addSinFunc maxX' maxY')
		(const $ const 0, gen)
	rez = A.array ((0, 0), (maxX, maxY)) [((x, y), cellFromCoords 
		(fromIntegral x) (fromIntegral y)) 
		| x <- [0 .. maxX], y <- [0 .. maxY]]
	normalize (l, r) (l', r') x = l' + (x - l) * (r' - l') / (r - l)
	cellFromCoords x y = Cell {
		terrain = Empty,
		height = uniformFromList (max 0 $ min 0.99 
			$ normalize (-cntSinF, cntSinF) (-1, 2) $ sinf x y) [0..9]
	}
	
safety :: World -> World
safety w = w {
	units' = (units' w) {list = M.singleton (xFirst w, yFirst w) $ getFirst w},
	message = [(msgSafety, bLUE)],
	items = [],
	action = Move,
	wave = wave w + 1,
	chars = S.empty,
	stdgen = newGen,
	worldmap = newMap,
	stepsBeforeWave = 2
} where (newMap, newGen) = getMap $ stdgen w

speed :: Int -> Monster -> Monster
speed x m = m {slowness = slowness m - x}

radiation :: Int -> Monster -> Monster
radiation sp m = m {parts = map (\p -> p {regRate = regRate p - sp}) $ parts m}

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

getGarbageCollector :: MonsterGen		  
getGarbageCollector = getMonster (getEatAI CollectorAI)
	[(getBody 1, (20, 40)), 
	 (getHead 1, (10, 30)),
	 (getLeg  1, ( 8, 12)),
	 (getLeg  1, ( 8, 12)),
	 (getArm  1, ( 8, 12)),
	 (getArm  1, ( 8, 12))]
	 17 ((2,4), 0.4) emptyInv 100 100

getGolem :: MonsterGen
getGolem = getMonster (getPureAI GolemAI)
	[(getBody 1, (10, 30)), 
	 (getHead 1, ( 8, 12)),
	 (getLeg  1, ( 3,  7)),
	 (getLeg  1, ( 3,  7)),
	 (getArm  1, ( 2,  6)),
	 (getArm  1, ( 2,  6))]
	18 ((2,4), 0.3) emptyInv 100 10000
		
spawnGolem :: Int -> Int -> World -> World
spawnGolem x y w = 
	if isEmpty w x y && correct
	then spawnMon getGolem x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		correct = any filterfun $ items w
		newItems = filter (not . filterfun) $ items w

spawnGolemsAround :: World -> World
spawnGolemsAround = fooAround spawnGolem

addNutr :: Int -> Monster -> Monster
addNutr n mon = changeTemp Nutrition 
	((+n) <$> temp mon !! fromEnum Nutrition) mon
