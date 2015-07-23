module Init where

import Data.Const
import Data.Define
import Utils.Monsters
--import Utils.Items
import Items.Stuff
import Monsters.Parts
import IO.Colors
import IO.Texts
import MapGen

import System.Random (StdGen)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Functor ((<$>))
		
initUnits :: Units
initUnits = Units {
	xF = x',
	yF = y',
	getFirst' = getPlayer,
	list = M.singleton (x', y') getPlayer
} where
	x' = div maxX 2
	y' = div maxY 2

initWorld :: MapGenType -> String -> StdGen -> World
initWorld mapgen username gen = World {
	worldmap = worldmap',
	units' = initUnits,
	message = [(msgWelcome username, bLUE)],
	items = [],
	action = Move,
	stdgen = newStdGen,
	wave = 1,
	chars = S.empty,
	prevAction = ' ',
	shift = 0,
	slot = toEnum 0,
	xInfo = 0,
	yInfo = 0,
	numToSplit = 0,
	showMode = ColorMonsters,
	mapType = mapgen
} where (worldmap', newStdGen) = runMap mapgen gen

getPlayer :: Monster
getPlayer = Monster {
	ai = You,
	parts = zipWith ($) 
		[getBody 1 40, 
		 getHead 1 30, 
		 getLeg  2 20, 
		 getLeg  2 20, 
		 getArm  2 20, 
		 getArm  2 20]
		 [0..],
	name = "You",
	stddmg = ((1,10), 0.2), -- avg 4.4
	inv = M.singleton 'a' (pickAxe, 1),
	slowness = 100,
	time = 100,
	res = const 0 <$> (getAll :: [Elem]),
	intr = const 0 <$> (getAll :: [Intr]),
	temp = startTemps 50,
	idM = 0,
	xp = 1
}

showMapChoice :: IO MapGenType
showMapChoice = do
	putStrLn "Choose a map:"
	putStrLn "a - sum of 30 sinuses (looks like aperiodic)"
	putStrLn "b - sum of 3 sinuses"
	putStrLn "c - flat map with height = 9"
	putStrLn "d - flat map with height = 0"
	putStrLn "e - averaged random map"
	putStrLn "f - double averaged random map"
	putStrLn "g - map with mountains and large valleys"
	putStrLn "h - (g) with rivers"
	putStrLn "i - (g) with swamps"
	c <- getLine
	case c of
		"a" -> return Sin30
		"b" -> return Sin3
		"c" -> return FlatHigh
		"d" -> return FlatLow
		"e" -> return AvgRandom
		"f" -> return Avg2Random
		"g" -> return Mountains
		"h" -> return MountainsWater
		"i" -> return MountainsSwamp
		_ ->  do
			putStrLn "Unknown map!"
			showMapChoice
