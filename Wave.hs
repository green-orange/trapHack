module Wave where

import Data
import Monsters

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
		(p, g') = randomR (0.0, 5.0) g
		frac = p - fromIntegral (floor p)
		(genM, d) = case floor p of
			0 -> (getHomunculus frac, 2)
			1 -> (getBeetle     frac, 3)
			2 -> (getBat        frac, 1)
			3 -> (getHunter     frac, 5)
			4 -> (getIvy        frac, 8)
		(oldWave, g'') = genWave (n - d) g'

newWave :: World -> World
newWave (World oldUnits message items action oldStdgen  wave      toPick store worldmap dirs) =
		 World units    message items action stdgen    (wave + 1) toPick store worldmap dirs where
	(units, stdgen) = addWave wave (oldUnits, oldStdgen)
