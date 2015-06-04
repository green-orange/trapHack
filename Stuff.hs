module Stuff where

import Data
import Utils4stuff
import Utils4all
import Utils4mon

import System.Random

deathDrop :: String -> StdGen -> ([(Char, Object, Int)], StdGen)
deathDrop "Homunculus" = genDeathDrop [((wandOfStriking 3),
	(\p ->
		if p < 0.3
		then 0
		else if p < 0.9
		then 1
		else 2
	)), (potionOfHealing,
	(\p ->
		if p < 0.3
		then 0
		else if p < 0.9
		then 1
		else 2))]
deathDrop "Beetle" = genDeathDrop [(potionOfIntellect,
	(\p -> 
		if p < (0.8 :: Float)
		then (0 :: Int)
		else 1))]
deathDrop "Bat" = genDeathDrop [(potionOfHealing,
	(\p -> 
		if p < 0.5
		then 1
		else 2))]
deathDrop _ = (\p -> ([], p))

genDeathDrop :: [(Object, (Float -> Int))] -> StdGen -> ([(Char, Object, Int)], StdGen)
genDeathDrop [] g = ([], g)
genDeathDrop xs g = (zipWith (\x (o,n) -> (x,o,n)) notAlphabet ys, g') where
	(ys, g') = (foldr1 (.*) $ map genDeathDropOne xs) g
	
genDeathDropOne :: (Object, (Float -> Int)) -> StdGen -> ([(Object, Int)], StdGen)
genDeathDropOne (obj, f) g = ([(obj, 1) | i <- [0..n-1]], g') where
	p :: Float
	(p, g') = randomR (0.0, 1.0) g
	n = f p
infixr 0 .*
(.*) :: (StdGen -> ([a], StdGen)) -> (StdGen -> ([a], StdGen)) -> (StdGen -> ([a], StdGen))
(f .* g) x = (l1 ++ l2, x'') where
	(l2, x')  = g x
	(l1, x'') = f x'

potionOfHealing :: Object
potionOfHealing = Potion {
	title = "potion of healing",
	act = healParts bODY 10
}

potionOfIntellect :: Object
potionOfIntellect = Potion {
	title = "potion of intellect",
	act = upgradeParts hEAD 5
}

potionOfMutation :: Object
potionOfMutation = lol

wandOfStriking :: Int -> Object
wandOfStriking ch = Wand {
	title = "wand of striking",
	act = dmgAll $ Just 5,
	range = 5,
	charge = ch
}

bearTrap :: Object
bearTrap = Trap {
	title = "bear trap",
	num = bEARTRAP
}

trapFromTerrain :: Terrain -> Object
trapFromTerrain t
	| t == bEARTRAP = bearTrap
	| otherwise = error "unlnown trap"
	
isUntrappable :: Terrain -> Bool
isUntrappable = (==) bEARTRAP
 
