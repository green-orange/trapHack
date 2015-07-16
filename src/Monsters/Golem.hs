module Monsters.Golem where

import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Monsters
import Monsters.Parts
import Monsters.Move
import Monsters.Monsters
import Monsters.AIrepr

import qualified Data.Map as M
import Data.Maybe (isJust)

golemAI :: AIfunc
golemAI _ _ _ world = case nears of
	[] -> world
	x:_ -> fst $ uncurry moveFirst x world
	where
		xNow = xFirst world
		yNow = yFirst world
		needToAttack (dx, dy) = isJust mons && isEnemy mon where
			mons = M.lookup (xNow + dx, yNow + dy) $ units world
			Just mon = mons
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
