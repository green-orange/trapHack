module Stuff where

import Data
import Utils4stuff
import Utils4all
import Utils4mon
import Random
import Monsters
import HealDamage
import GarbageCollector

import System.Random

deathDrop :: String -> StdGen -> ([Inv], StdGen)
deathDrop "Homunculus" = genRandomWand $ bound [0.6]
deathDrop "Beetle" = genRandomPotion $ bound [0.5]
deathDrop "Bat" = genRandomPotion $ bound [0.3, 0.8]
deathDrop "Hunter" = 
	genRandomFooByChar (notAlphabet !! 0) tRAPS (bound [0.3, 0.8]) .+
	genRandomFooByChar (notAlphabet !! 1) wEAPONS (bound [0.6])
deathDrop "Ivy" = genRandomScroll $ bound [0.7, 0.9]
deathDrop "Accelerator" = genRandomScroll $ bound [0.3, 0.6, 0.9]
deathDrop _ = (\p -> ([], p))

bound :: [Float] -> Float -> Int
bound list p = bound' list p 0 where
	bound' []     p n = n
	bound' (x:xs) p n = 
		if p < x
		then n
		else bound' xs p (n + 1)
		
(.+) :: (a -> ([b], a)) -> (a -> ([b], a)) -> (a -> ([b], a))
(f .+ g) x = (l1 ++ l2, x'') where
	(l1, x' ) = g x
	(l2, x'') = f x'

genDeathDrop = genDeathDropByAlph notAlphabet

genDeathDropByAlph :: [Char] -> [(Object, (Float -> Int))] -> StdGen -> ([Inv], StdGen)
genDeathDropByAlph _ [] g = ([], g)
genDeathDropByAlph alph xs g = (zipWith (\x (o,n) -> (x,o,n)) alph ys, g') where
	(ys, g') = (foldr1 (.+) $ map genDeathDropOne xs) g

pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation]
genRandomPotion = genRandomFoo pOTIONS

tRAPS = [bearTrap, fireTrap]
genRandomTrap = genRandomFoo tRAPS

lAUNCHERS = [shortbow, bow, longbow]
genRandomLauncher = genRandomFoo lAUNCHERS

wEAPONS = [dagger, shortsword, sword]
genRandomWeapon = genRandomFoo wEAPONS

sCROLLS = [scrollOfFire, scrollOfAnimation, scrollOfCollection]
genRandomScroll = genRandomFoo sCROLLS

wANDS = [wandOfStriking 3, wandOfStupidity 2]
genRandomWand = genRandomFoo wANDS

genRandomFoo = genRandomFooByChar $ head notAlphabet

genRandomFooByChar :: Char -> [Object] -> (Float -> Int) -> StdGen -> ([Inv], StdGen)
genRandomFooByChar c foos f gen =
	if cnt == 0
	then ([], gen')
	else ([(c, obj, cnt)], gen') where
	(p, gen') = randomR (0.0, fromIntegral $ length foos) gen
	obj = foos !! (floor p)
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

scrollOfFire :: Object
scrollOfFire = Scroll {
	title = "scroll of fire",
	actw = fireAround 1 (5, 10)
}

scrollOfAnimation :: Object
scrollOfAnimation = Scroll {
	title = "scroll of animation",
	actw = animateAround
}

scrollOfCollection :: Object
scrollOfCollection = Scroll {
	title = "scroll of collection",
	actw = randomSpawn getGarbageCollector
}

wandOfStriking :: Int -> Object
wandOfStriking ch = Wand {
	title = "wand of striking",
	act = unrandom $ dmgAll $ Just 10,
	range = 5,
	charge = ch
}

wandOfStupidity :: Int -> Object
wandOfStupidity ch = Wand {
	title = "wand of stupidity",
	act = unrandom stupidity,
	range = 3,
	charge = ch
}

bearTrap :: Object
bearTrap = Trap {
	title = "bear trap",
	num = bEARTRAP
}

fireTrap :: Object
fireTrap = Trap {
	title = "fire trap",
	num = fIRETRAP
}

arrow = Missile {
	title = "arrow",
	objdmg = dices (1,6) 0.2,
	launcher = "bow"
}

shortbow = Launcher {
	title = "short bow",
	count = 1,
	category = "bow"
}

bow = Launcher {
	title = "bow",
	count = 2,
	category = "bow"
}

longbow = Launcher {
	title = "longbow",
	count = 3,
	category = "bow"
}

dagger = Weapon {
	title = "dagger",
	objdmg = dices (1,12) 0.0
}

shortsword = Weapon {
	title = "shortsword",
	objdmg = dices (2,8) 0.1
}

sword = Weapon {
	title = "sword",
	objdmg = dices (2,10) 0.1
}

trapFromTerrain :: Terrain -> Object
trapFromTerrain t
	| t == bEARTRAP = bearTrap
	| t == fIRETRAP = fireTrap
	| otherwise = error "unknown trap"
 
