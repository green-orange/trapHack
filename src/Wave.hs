module Wave where

import Data
import Monsters
import MonsterList
import Ivy
import Forgotten
import Utils4mon
import DataMonster
import DataDef

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
	(ind, g') = randomR (0, 15) g
	genM = case ind of
		0  -> getHomunculus
		1  -> getBeetle
		2  -> getBat
		3  -> getHunter
		4  -> getIvy
		5  -> getAccelerator
		6  -> getTroll
		7  -> getWorm
		8  -> getFloatingEye
		9  -> getRedDragon
		10 -> getWhiteDragon
		11 -> getGreenDragon
		12 -> getForgottenBeast
		13 -> getSpider
		14 -> getSoldier
		15 -> getUmberHulk
		_  -> error "value error in function genWave"
	d = levelM $ nameFromGen genM
	(oldWave, g'') = genWave (n - d) g'

newWave :: World -> World
newWave w = w {units' = newUnits, stdgen = newStdGen, wave = wave w + 1} where
	(newUnits, newStdGen) = addWave (wave w) 
		$ addWaveFull (wave w) (units' w, stdgen w)
