module Utils4step where

import Data
import Utils4all
import Changes
import Stuff
import Utils4mon
import Utils4stuff
import Wave

import System.Random (StdGen)

bIGpAUSE = 100
pAUSE    = 3

wait :: Int -> Int
wait n = case mod n 10 of
	0 -> bIGpAUSE
	_ -> pAUSE

newWaveIf :: World -> World
newWaveIf world =
	if (not $ isPlayerNow world) ||
		(length $ filter (isSoldier. third) $ units world) * 8 > wave world
	then cycleWorld $ resetTime world
	else
		if stepsBeforeWave world > 0
		then (cycleWorld $ resetTime world)
			{stepsBeforeWave = stepsBeforeWave world - 1}
		else if stepsBeforeWave world == 0
		then (changeAction ' ' $ addMessage ("Squad #" 
			++ show (wave world) ++ " landed around you!", rED) 
			$ newWave $ cycleWorld $ resetTime world) {stepsBeforeWave = -1}
		else (cycleWorld $ resetTime world) {stepsBeforeWave = wait $ wave world}

cycleWorld :: World -> World
cycleWorld w = actTrapFirst $ regFirst $ cleanFirst $ changeMons newUnits 
	$ addMessages (msgCleanParts $ third $ head newUnits) w
	where newUnits = cycle' $ units w

cleanFirst :: World -> World
cleanFirst w = changeMon (cleanParts $ getFirst w) w

remFirst :: World -> World
remFirst world = changeMons (tail $ units world) $ changeAction ' ' world

coordsPlayer :: World -> (Int, Int)
coordsPlayer w =
	if null yous
	then (-1, -1)
	else getCoords $ head yous
	where
		getCoords (x,y,_) = (x,y)
		yous = filter (\(_,_,x) -> (name x == "You")) $ units w

addDeathDrop :: Monster -> StdGen -> (Monster, StdGen)
addDeathDrop mon g = (changeInv (inv mon ++ newDrop) mon, newGen) where
	(newDrop, newGen) = deathDrop (name mon) g

tickDown :: World -> World
tickDown w = changeMon (tickDownMon $ getFirst w) w

resetTime :: World -> World
resetTime w = changeMon (resetTimeMon $ getFirst w) w

