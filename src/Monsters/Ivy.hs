module Monsters.Ivy where

import Data.Monster
import Data.Define
import Data.World
import Utils.Monsters
import Utils.Changes
import Monsters.Monsters
import Monsters.Parts
import Monsters.Move
import Monsters.AIrepr

import System.Random (randomR)

getIvy :: MonsterGen
getIvy = getMonster (getPureAI IvyAI) [(getMain 2, (5, 15))] 15
	((2,10), 0.0) emptyInv 400 100

ivyAI :: AIfunc
ivyAI xPlayer yPlayer peace world
	| abs dx <= 1 && abs dy <= 1 && not peace = fst $ moveFirst dx dy world
	| isEmpty world (xNow + dx') (yNow + dy')
		= spawnMon getIvy (xNow + dx') (yNow + dy') $ changeGen g'' world
	| otherwise = changeGen g'' $ killFirst world where
		xNow = xFirst world
		yNow = yFirst world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
