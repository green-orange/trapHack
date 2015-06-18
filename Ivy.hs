module Ivy where

import Data
import Monsters
import Utils4mon
import Random
import Parts
import Changes
import Move

import System.Random (randomR)
import Data.Map (empty)

getIvy :: Float -> MonsterGen
getIvy q = getMonster ivyAI [getMain 2 $ uniform q 5 15] "Ivy"
	(dices (2,10) 0) (const empty) 600

ivyAI :: AIfunc
ivyAI xPlayer yPlayer world = 
	if abs dx <= 1 && abs dy <= 1
	then moveFirst dx dy world
	else if isEmpty world (xNow + dx') (yNow + dy')
	then spawnMon (getIvy q) (xNow + dx') (yNow + dy') $ changeGen g''' world
	else killFirst world where
		xNow = xFirst world
		yNow = yFirst world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
		(q, g''') = randomR (0.0, 1.0) g''
