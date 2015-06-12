module Wave where

import Data
import Monsters
import MonsterList

import System.Random (StdGen, randomR)

addWave :: Int -> ([Unit], StdGen) -> ([Unit], StdGen)
addWave n pair@(units, g) = 
	if null ms
	then addWave n (units, g')
	else addMonsters ms (units, g')
	where
		(ms, g') = genWave n g
		
genWave :: Int -> StdGen -> ([MonsterGen], StdGen)
genWave n g = 
	if n <= 0
	then ([], g)
	else if d > n
	then (oldWave, g'')
	else (genM : oldWave, g'')
	where
		p :: Float
		(p, g') = randomR (0.0, 7.0) g
		frac = p - fromIntegral (floor p)
		(genM, d) = case floor p of
			0 -> (getHomunculus  frac, 2)
			1 -> (getBeetle      frac, 3)
			2 -> (getBat         frac, 1)
			3 -> (getHunter      frac, 4)
			4 -> (getIvy         frac, 3)
			5 -> (getAccelerator frac, 3)
			6 -> (getTroll       frac, 3)
		(oldWave, g'') = genWave (n - d) g'

newWave :: World -> World
newWave w = w {units = newUnits, stdgen = newStdGen, wave = wave w + 1} where
	(newUnits, newStdGen) = addWave (wave w) (units w, stdgen w)
