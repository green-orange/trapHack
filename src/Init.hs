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
import Data.Char (isDigit)
import Control.Monad (when)
		
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
	putStrLn "a - sum of 30 sinuses, DEFAULT"
	putStrLn "b - flat map with height = 9"
	putStrLn "c - averaged random map"
	putStrLn "d - map with mountains and large valleys"
	putStrLn "e - (g) with rivers"
	putStrLn "f - (g) with swamps"
	putStrLn "* - customize map"
	c <- getLine
	case c of
		"a" -> return $ pureMapGen Sin30
		"b" -> return $ pureMapGen $ Flat 9
		"c" -> return $ MapGenType Random 1 NoWater
		"d" -> return $ pureMapGen Mountains
		"e" -> return $ MapGenType Mountains 0 $ Rivers 50
		"f" -> return $ MapGenType Mountains 0 $ Swamp 3
		"*" -> customMapChoice
		_ ->  return $ pureMapGen Sin30

customMapChoice :: IO MapGenType
customMapChoice = do
	putStrLn "Choose a height generator: "
	putStrLn "a - sum of 30 sinuses (not really), DEFAULT"
	putStrLn "b - sum of 3 sinuses"
	putStrLn "c - random map"
	putStrLn "d - mountains"
	putStrLn "e - flat map (with customized height)"
	heigenStr <- getLine
	when (heigenStr == "e") $ putStrLn "Put height of the map (default: 9)"
	hei <- if heigenStr == "e" then getLine else return ""
	putStrLn "Choose averaging: (default: 0)"
	avgStr <- getLine
	putStrLn "Choose water: "
	putStrLn "a - without water, DEFAULT"
	putStrLn "b - rivers"
	putStrLn "c - swamps"
	waterStr <- getLine
	when (waterStr == "b") $ putStrLn "Put count of rivers (default: 50)"
	when (waterStr == "c") $ putStrLn "Put depth of swamps (default: 3)"
	waternum <-	if waterStr == "b" || waterStr == "c" then getLine
		else return ""
	return $ MapGenType (heigen heigenStr hei) (avg avgStr)
		(water waterStr waternum)
	where
		maybeReadNum :: Int -> String -> Int
		maybeReadNum def [] = def
		maybeReadNum def str = 
			if all isDigit str then read str else def
		heigen str1 str2 = case str1 of
			"a" -> Sin30
			"b" -> Sin3
			"c" -> Random
			"d" -> Mountains
			"e" -> Flat $ maybeReadNum 9 str2
			_ -> Sin30
		avg = maybeReadNum 0
		water str1 str2 = case str1 of
			"a" -> NoWater
			"b" -> Rivers $ maybeReadNum 50 str2
			"c" -> Swamp $ maybeReadNum 3 str2
			_ -> NoWater
	



