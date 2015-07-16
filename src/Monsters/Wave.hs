module Monsters.Wave where

import Data.Const
import Data.Monster
import Data.Define
import Utils.Monsters
import Monsters.Monsters
import Monsters.MonsterList
import Monsters.Forgotten
import Monsters.AI

import System.Random (StdGen, randomR)
import qualified Data.Map as M

nameFromGen :: MonsterGen -> String
nameFromGen mgen = name $ fst $ mgen lol

addWaveBy :: ([MonsterGen] -> (Units, StdGen) -> (Units, StdGen)) 
	-> Int -> (Units, StdGen) -> (Units, StdGen)
addWaveBy fun n (uns, g) = 
	if null ms
	then addWave n (uns, g')
	else fun ms (uns, g')
	where
		(ms, g') = genWave n g

addWave, addWaveFull :: Int -> (Units, StdGen) -> (Units, StdGen)
addWave = addWaveBy addMonsters
addWaveFull = addWaveBy addMonstersFull
		
levelW :: World -> Int
levelW w = M.foldr (+) 0 $ M.map (levelM . name) $ M.filter isSoldier 
	$ M.filterWithKey (\(x, y) _ -> abs (x - xPlayer) <= xSight 
	&& abs (y - yPlayer) <= ySight) $ units w where
	[((xPlayer, yPlayer), _)] = filter (\(_,m) -> name m == "You") 
		$ M.toList $ units w
		
genWave :: Int -> StdGen -> ([MonsterGen], StdGen)
genWave n g
	| n <= 0 = ([], g)
	| d > n = (oldWave, g'')
	| otherwise = (genM : oldWave, g'') where
	ind :: Int
	(ind, g') = randomR (0, length gens - 1) g
	gens = [getHomunculus, getBeetle, getBat, getHunter, getIvy,
		getAccelerator, getTroll, getWorm, getFloatingEye, getRedDragon, 
		getWhiteDragon, getGreenDragon, getForgottenBeast, getSpider, 
		getSoldier, getUmberHulk, getTree]
	genM = gens !! ind
	d = levelM $ nameFromGen genM
	(oldWave, g'') = genWave (n - d) g'

newWave :: World -> World
newWave w = w {units' = newUnits, stdgen = newStdGen, wave = wave w + 1} where
	(newUnits, newStdGen) = addWave (wave w) 
		$ addWaveFull (wave w) (units' w, stdgen w)
