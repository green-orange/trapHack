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
		p :: Int
		(p, g') = randomR (0, 3) g
		(genM, d) = case p of
			0 -> (getHomunculus, 2)
			1 -> (getBeetle    , 3)
			2 -> (getBat       , 1)
			3 -> (getHunter    , 5)
		(oldWave, g'') = genWave (n - d) g'

newWave :: World -> World
newWave (World oldUnits message items action oldStdgen  wave      toPick store worldmap dirs) =
		 World units    message items action stdgen    (wave + 1) toPick store worldmap dirs where
	(units, stdgen) = addWave wave (oldUnits, oldStdgen)
