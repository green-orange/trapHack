module Monsters where

import Data
import Changes
import Parts
import Messages

import System.Random (StdGen, randomR)
import Data.Map (empty, keys)
	
getMonster :: AIfunc -> [Int -> Part] -> String -> StdDmg -> InvGen -> Int -> MonsterGen
getMonster ai' ps name' stddmg' inv' slow' g = (Monster {
	ai = AI ai',
	parts = zipWith ($) ps [0..],
	name = name',
	stddmg = stddmg',
	inv = inv' p,
	slowness = slow',
	time = slow',
	poison = Nothing,
	res = [0,0..],
	intr = [0,0..]
}, newGen) where
	p :: Float
	(p, newGen) = randomR (0.0, 1.0) g

getDummy :: Int -> Float -> MonsterGen
getDummy n _ = getMonster (\_ _ w -> w) [getMain 1 n] "Dummy" lol (const empty) 100

addMonsters :: [MonsterGen] -> (Units, StdGen) -> (Units, StdGen)
addMonsters gens pair = foldr addMonster pair gens

addMonster :: MonsterGen -> (Units, StdGen) -> (Units, StdGen)
addMonster gen (uns, g) = 
	if isCorrect
	then (insertU (x, y) (mon {time = time $ getFirst' uns}) uns, g3)
	else addMonster gen (uns, g3)
	where
		(x, g1) = randomR (0, maxX) g
		(y, g2) = randomR (0, maxY) g1
		(mon, g3) = gen g2
		isCorrect = not $ any (\(a,b) -> a == x && b == y) $ keys $ list uns
	
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
		hp' = sum $ map mapfun $ items w
		newItems = filter (not . filterfun) $ items w
		
fooAround :: (Int -> Int -> World -> World) -> World -> World
fooAround foo w = foldr ($) w $ [foo] >>= applToNear x >>= applToNear y where
	x = xFirst w
	y = yFirst w
	applToNear t f = map f [t-1, t, t+1]

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
		then maybeAddMessage "There is no place for the garbage collector!" w
		else changeGen g $ spawnMon mgen xR yR w
	(r, g) = randomR (0, length emptyNeighbors - 1) $ stdgen w
	(xR, yR) = emptyNeighbors !! r


