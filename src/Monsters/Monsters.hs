module Monsters.Monsters where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Data.ID
import Utils.Changes
import Utils.Monsters
import Monsters.Parts
import Monsters.AIrepr
import IO.Messages
import IO.Texts

import System.Random (StdGen, randomR)
import qualified Data.Map as M
import Data.Functor ((<$>))

-- | list of monster names sorted by ID
monNames :: [String]
monNames = ["You", "Homunculus", "Beetle", "Bat", "Hunter", "Accelerator", "Troll",
	"Worm", "Floating eye", "Red dragon", "White dragon", "Green dragon",
	"Spider", "Soldier", "Umber hulk", "Ivy", "Tail", "Garbage collector",
	"Golem", "Dummy", "Rock", "Forgotten beast", "Tree", "Bot", "Bee", "Bush"]

-- | get monster by Ai representation, list of part getters and hps,
-- id, damage getter, inventory generator, slowness, start nutrition
-- and random number generator
getMonster :: AIrepr -> [(Int -> Int -> Part, (Int, Int))]
	-> Int -> ((Int, Int), Float) -> InvGen -> Int -> Int -> MonsterGen
getMonster ai' ps id' stddmg' inv' slow' nutr g = (Monster {
	ai = AI ai',
	parts = zipWith ($) partGens [0..],
	idM = id',
	name = if id' >= 0 && id' < length monNames then monNames !! id'
		else putWE "getMonster",
	stddmg = stddmg',
	inv = newInv,
	slowness = slow',
	time = slow',
	res = const 0 <$> (getAll :: [Elem]),
	intr = const 0 <$> (getAll :: [Intr]),
	temp = startTemps nutr,
	xp = 1
}, g'') where
	addHPs :: [(Int -> Int -> Part, (Int, Int))] 
		-> StdGen -> ([Int -> Part], StdGen)
	addHPs [] gen = ([], gen)
	addHPs ((genPart, pair):xs) gen = (newPart:oldParts, newGen) where
		(oldParts, oldGen) = addHPs xs gen
		(newHP, newGen) = randomR pair oldGen
		newPart = genPart newHP
	(partGens, g') = addHPs ps g
	(newInv, g'') = inv' g'

-- | get dummy from Scroll of Animation; it really do nothing
getDummy :: Int -> Float -> MonsterGen
getDummy n _ = getMonster (getPureAI NothingAI) [(getMain 1, (n, n))] idDum 
	((0, 0), 0.0) emptyInv 10000 1

addMonsters, addMonstersFull :: [MonsterGen] -> (Units, StdGen) -> (Units, StdGen)
-- | add list of monster generators to given units in sight of the player
addMonsters gens pair = foldr addMonster pair gens
-- | add list of monster generators to given units in the random place
addMonstersFull gens pair = foldr addMonsterFull pair gens

-- | add one monster to given units in sight of the player
addMonster :: MonsterGen -> (Units, StdGen) -> (Units, StdGen)
addMonster gen (uns, g) = 
	if isCorrect
	then (insertU (x, y) (mon {time = time $ getFirst' uns}) uns, g3)
	else addMonster gen (uns, g3)
	where
		(x, g1) = randomR (max 0 (xPlayer - xSight), 
			min maxX (xPlayer + xSight)) g
		(y, g2) = randomR (max 0 (yPlayer - ySight), 
			min maxY (yPlayer + ySight)) g1
		(mon, g3) = gen g2
		isCorrect = M.notMember (x, y) $ list uns
		[((xPlayer, yPlayer), _)] = filter (\(_,m) -> name m == "You") 
			$ M.toList $ list uns

-- | add one monster generator to given units in the random place
addMonsterFull :: MonsterGen -> (Units, StdGen) -> (Units, StdGen)
addMonsterFull gen (uns, g) = 
	if isCorrect
	then (insertU (x, y) (mon {time = time $ getFirst' uns}) uns, g3)
	else addMonster gen (uns, g3)
	where
		(x, g1) = randomR (0, maxX) g
		(y, g2) = randomR (0, maxY) g1
		(mon, g3) = gen g2
		isCorrect = not $ any (\(a,b) -> a == x && b == y) $ M.keys $ list uns

-- | animate all objects in the given cell
animate :: Int -> Int -> World -> World
animate x y w = 
	if isEmpty w x y && hp' > 0
	then spawnMon (getDummy hp' $ putWE "animate") x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		mapfun arg@(_, _, _, n) = 
			if filterfun arg
			then n
			else 0
		hp' = sum $ mapfun <$> items w
		newItems = filter (not . filterfun) $ items w

-- | do something with all cells around given
fooAround :: (Int -> Int -> World -> World) -> World -> World
fooAround foo w = foldr ($) w $ [foo] >>= applToNear x >>= applToNear y where
	x = xFirst w
	y = yFirst w
	applToNear t f = f <$> [t-1, t, t+1]

-- | animate all items around given cell
animateAround :: World -> World
animateAround = fooAround animate

-- | spawn given monster in the random cell next to the current monster
randomSpawn :: MonsterGen -> World -> World
randomSpawn mgen w = newWorld where
	x = xFirst w
	y = yFirst w
	neighbors = [(x', y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]]
	emptyNeighbors = filter (uncurry $ isEmpty w) neighbors
	newWorld = 
		if null emptyNeighbors
		then maybeAddMessage msgCantSpawnGC w
		else spawnMon mgen xR yR w {stdgen = g}
	(r, g) = randomR (0, length emptyNeighbors - 1) $ stdgen w
	(xR, yR) = emptyNeighbors !! r

