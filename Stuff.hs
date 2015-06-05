module Stuff where

import Data
import Utils4stuff
import Utils4all
import Utils4mon
import Random

import System.Random

deathDrop :: String -> StdGen -> ([(Char, Object, Int)], StdGen)
deathDrop "Homunculus" = genDeathDrop [((wandOfStriking 3),
	(\p ->
		if p < 0.7
		then 0
		else if p < 0.95
		then 1
		else 2
	)), (potionOfHealing,
	(\p ->
		if p < 0.5
		then 0
		else if p < 0.9
		then 1
		else 2))]
deathDrop "Beetle" = genRandomPotion
	(\p -> 
		if p < 0.5
		then 0
		else 1)
deathDrop "Bat" = genRandomPotion
	(\p -> 
		if p < 0.3
		then 0
		else if p < 0.8
		then 1
		else 2)
deathDrop "Hunter" = genDeathDrop [(bearTrap, 
	(\p -> 
		if p < 0.3
		then 0
		else 1)
	)]
deathDrop _ = (\p -> ([], p))

genDeathDrop :: [(Object, (Float -> Int))] -> StdGen -> ([(Char, Object, Int)], StdGen)
genDeathDrop [] g = ([], g)
genDeathDrop xs g = (zipWith (\x (o,n) -> (x,o,n)) notAlphabet ys, g') where
	(ys, g') = (foldr1 (.*) $ map genDeathDropOne xs) g

pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation]

genRandomPotion :: (Float -> Int) -> StdGen -> ([(Char, Object, Int)], StdGen)
genRandomPotion f g =
	if cnt == 0
	then ([], g')
	else ([(head notAlphabet, obj, cnt)], g') where
	(p, g') = randomR (0.0, fromIntegral $ length pOTIONS) g
	obj = pOTIONS !! (floor p)
	cnt = f $ p - fromIntegral (floor p)
	
genDeathDropOne :: (Object, (Float -> Int)) -> StdGen -> ([(Object, Int)], StdGen)
genDeathDropOne (obj, f) g = 
	if n == 0
	then ([], g')
	else if isStackable obj
	then ([(obj, n)], g')
	else (replicate n (obj, 1), g')
	where
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
	act = unrandom $ healParts bODY 10
}

potionOfIntellect :: Object
potionOfIntellect = Potion {
	title = "potion of intellect",
	act = unrandom $ upgradeParts hEAD 5
}

unrandom :: (a -> a) -> (a, x) -> (a, x)
unrandom f (a, x) = (f a, x)

potionOfMutation :: Object
potionOfMutation = Potion {
	title = "potion of mutation",
	act = addRandomPart
}

wandOfStriking :: Int -> Object
wandOfStriking ch = Wand {
	title = "wand of striking",
	act = unrandom $ dmgAll $ Just 5,
	range = 5,
	charge = ch
}

bearTrap :: Object
bearTrap = Trap {
	title = "bear trap",
	num = bEARTRAP
}

arrow = Missile {
	title = "arrow",
	objdmg = dices [(1,10)] 0.2,
	launcher = "bow"
}

bow = Launcher {
	title = "bow",
	count = 1,
	category = "bow"
}

longbow = Launcher {
	title = "longbow",
	count = 3,
	category = "bow"
}

trapFromTerrain :: Terrain -> Object
trapFromTerrain t
	| t == bEARTRAP = bearTrap
	| otherwise = error "unknown trap"
	
isUntrappable :: Terrain -> Bool
isUntrappable = (==) bEARTRAP
 
