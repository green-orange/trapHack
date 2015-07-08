module Golem where

import Parts
import Move
import Utils4mon
import Monsters
import Changes
import AIrepr
import DataWorld
import DataMonster
import DataDef

import qualified Data.Map as M

golemAI :: AIfunc
golemAI _ _ world = 
	if null nears
	then world
	else (uncurry moveFirst $ head nears) world where
		xNow = xFirst world
		yNow = yFirst world
		needToAttack (dx, dy) = not (M.null mons) && isEnemy mon where
			mons = M.filterWithKey (\(x,y) _ -> x == xNow + dx && y == yNow + dy)
				$ units world
			mon = getFirst world
		d = [-1, 0, 1]
		nears = filter needToAttack [(dx, dy) | dx <- d, dy <- d]

getGolem :: MonsterGen
getGolem = getMonster (getPureAI GolemAI)
	[(getBody 1, (10, 30)), 
	 (getHead 1, ( 8, 12)),
	 (getLeg  1, ( 3,  7)),
	 (getLeg  1, ( 3,  7)),
	 (getArm  1, ( 2,  6)),
	 (getArm  1, ( 2,  6))]
	18 (((2,4), 0.3)) emptyInv 100
		
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
