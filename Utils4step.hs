module Utils4step where

import Data
import Utils4all
import Changes
import Stuff
import Utils4mon
import Utils4stuff
import Wave

import System.Random (StdGen)

wAIT = 2

newWaveIf :: World -> World
newWaveIf world =
	if (not $ isPlayerNow world) ||
		(length $ filter (isSoldier. third) $ units world) * 8 > wave world
	then cycleWorld $ resetTime world
	else if (length $ store world) > 0
	then
		if (head $ store world) /= toEnum 0
		then changeStore [pred $ head $ store world] $ cycleWorld $ resetTime world
		else changeStore [] $ changeAction ' ' $ addMessage ("Squad #" 
			++ show (wave world) ++ " landed around you!", rED) 
			$ newWave $ cycleWorld $ resetTime world
	else changeStore [toEnum wAIT] $ cycleWorld $ resetTime world

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

