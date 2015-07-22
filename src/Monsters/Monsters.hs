module Monsters.Monsters where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Monsters
import Monsters.Parts
import Monsters.AIrepr
import IO.Messages
import IO.Texts

import System.Random (StdGen, randomR)
import qualified Data.Map as M
import Data.Functor ((<$>))
	
getMonster :: AIrepr -> [(Int -> Int -> Part, (Int, Int))]
	-> Int -> ((Int, Int), Float) -> InvGen -> Int -> Int -> MonsterGen
getMonster ai' ps id' stddmg' inv' slow' nutr g = (Monster {
	ai = AI ai',
	parts = zipWith ($) partGens [0..],
	idM = id',
	name = if id' >= 0 && id' < length monNames then monNames !! id'
		else error $ msgWE "getMonster",
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

getDummy :: Int -> Float -> MonsterGen
getDummy n _ = getMonster (getPureAI NothingAI) [(getMain 1, (n, n))] 19 
	((0, 0), 0.0) emptyInv 10000 1

addMonsters, addMonstersFull :: [MonsterGen] -> (Units, StdGen) -> (Units, StdGen)
addMonsters gens pair = foldr addMonster pair gens
addMonstersFull gens pair = foldr addMonsterFull pair gens

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
		isCorrect = not $ any (\(a,b) -> a == x && b == y) $ M.keys $ list uns
		[((xPlayer, yPlayer), _)] = filter (\(_,m) -> name m == "You") 
			$ M.toList $ list uns

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
	
animate :: Int -> Int -> World -> World
animate x y w = 
	if isEmpty w x y && hp' > 0
	then spawnMon (getDummy hp' lol) x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		mapfun arg@(_, _, _, n) = 
			if filterfun arg
			then n
			else 0
		hp' = sum $ mapfun <$> items w
		newItems = filter (not . filterfun) $ items w
		
fooAround :: (Int -> Int -> World -> World) -> World -> World
fooAround foo w = foldr ($) w $ [foo] >>= applToNear x >>= applToNear y where
	x = xFirst w
	y = yFirst w
	applToNear t f = f <$> [t-1, t, t+1]

animateAround :: World -> World
animateAround = fooAround animate
	
randomSpawn :: MonsterGen -> World -> World
randomSpawn mgen w = newWorld where
	x = xFirst w
	y = yFirst w
	neighbors = [(x', y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]]
	emptyNeighbors = filter (uncurry $ isEmpty w) neighbors
	newWorld = 
		if null emptyNeighbors
		then maybeAddMessage msgCantSpawnGC w
		else changeGen g $ spawnMon mgen xR yR w
	(r, g) = randomR (0, length emptyNeighbors - 1) $ stdgen w
	(xR, yR) = emptyNeighbors !! r

