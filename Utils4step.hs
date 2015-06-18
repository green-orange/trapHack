module Utils4step where

import Data
import Changes
import Stuff
import Utils4mon
import Utils4stuff
import Wave

import System.Random (StdGen)
import Data.List (sort)
import qualified Data.Map as M

bIGpAUSE, pAUSE :: Int
bIGpAUSE = 100
pAUSE    = 3

minSnd :: (Ord b) => (a,b) -> (a,b) -> (a,b)
minSnd x y = if snd x > snd y then y else x

minValue :: (Ord k, Ord a) => M.Map k a -> (k, a)
minValue m = foldr1 minSnd $ M.toList m
	
minimumOn :: (Ord b, Ord k) => (a -> b) -> M.Map k a -> (k, a)
minimumOn f m = (k, m M.! k) where
	k = fst $ minValue $ M.map f m

wait :: Int -> Int
wait n = case mod n 10 of
	0 -> bIGpAUSE
	_ -> pAUSE
	
almostTime :: Monster -> Int
almostTime mon = 
	if alive mon
	then time mon
	else 0

updateFirst :: World -> World
updateFirst w = changeMons newUnits w where
	newUnits = (units' w) {
		xF = x,
		yF = y,
		getFirst' = monNew
	}
	((x, y), monNew) = minimumOn almostTime $ units w

newWaveIf :: World -> World
newWaveIf world =
	if (not $ isPlayerNow world) ||
		weigthW world * 3 > wave world
	then newWorld
	else
		if stepsBeforeWave world > 0
		then newWorld
			{stepsBeforeWave = stepsBeforeWave world - 1}
		else if stepsBeforeWave world == 0
		then (changeAction ' ' $ addMessage ("Squad #" 
			++ show (wave world) ++ " landed around you!", rED) 
			$ newWave $ newWorld) {stepsBeforeWave = -1}
		else newWorld {stepsBeforeWave = wait $ wave world}
	where
		newWorld = cycleWorld world
		
cycleWorld :: World -> World
cycleWorld w = actTrapFirst $ regFirst $ cleanFirst $ changeMons newUnits 
	$ addMessages (msgCleanParts monNew) newWorld where
		newUnits = (units' newWorld) {
			xF = x,
			yF = y,
			getFirst' = monNew
		}
		((x, y), monNew) = minimumOn almostTime $ units newWorld
		newWorld = tickFirst w

cleanFirst :: World -> World
cleanFirst w = changeMon (cleanParts $ getFirst w) w

remFirst :: World -> World
remFirst world = updateFirst $ changeMons (deleteU (xFirst world, yFirst world) $ units' world) 
	$ changeAction ' ' world

coordsPlayer :: World -> (Int, Int)
coordsPlayer w =
	if null yous
	then (-1, -1)
	else fst $ head yous
	where
		yous = filter (\q -> (name (snd q) == "You")) $ M.toList $ units w

addDeathDrop :: Monster -> StdGen -> (Monster, StdGen)
addDeathDrop mon g = (changeInv (M.union (inv mon) newDrop) mon, newGen) where
	(newDrop, newGen) = deathDrop (name mon) g

tickFirst :: World -> World
tickFirst w = changeMon (tickFirstMon $ getFirst w) w

listOfValidChars :: (Object -> Bool) -> World -> [Char]
listOfValidChars f world = sort $ foldr (:) [] $ M.keys 
	$ M.filter (f . fst) $ inv $ getFirst world
	
doIfCorrect :: (World, Bool) -> Either World String
doIfCorrect (rez, correct) = 
	if correct
	then Left $ newWaveIf rez
	else Left rez
