module Golem where

import Data
import Random
import Parts
import Move
import Utils4mon
import Monsters
import Changes

import Data.Map (empty)
import System.Random (randomR)

golemAI :: AIfunc
golemAI world _ _ = 
	if null nears
	then world
	else (uncurry moveFirst $ head nears) world where
		(xNow, yNow, _) = head $ units world
		needToAttack (dx, dy) = not (null mons) && isEnemy mon where
			mons = filter (\(x,y,_) -> x == xNow + dx && y == yNow + dy)
				$ units world
			(_,_,mon) = head mons
		d = [-1, 0, 1]
		nears = filter needToAttack [(dx, dy) | dx <- d, dy <- d]
		
getGolem q = getMonster golemAI
	[getBody 1 $ uniform q 10 30, 
	 getHead 1 $ uniform q  8 12,
	 getLeg  1 $ uniform q  3  7,
	 getLeg  1 $ uniform q  3  7,
	 getArm  1 $ uniform q  2  6,
	 getArm  1 $ uniform q  2  6]
	"Golem" (dices (2,4) 0.3) (const empty) 100
		
spawnGolem :: Int -> Int -> World -> World
spawnGolem x y w = 
	if isEmpty w x y && correct
	then changeGen g $ spawnMon (getGolem p) x y $ w {items = newItems}
	else w where
		filterfun (x', y', _, _) = x == x' && y == y'
		correct = any filterfun $ items w
		newItems = filter (not . filterfun) $ items w
		(p, g) = randomR (0.0, 1.0) $ stdgen w

spawnGolemsAround = fooAround spawnGolem
