module Stuff where

import Data
import Utils4stuff
import Utils4mon
import Random
import Monsters
import HealDamage
import GarbageCollector
import Parts
import Golem

import System.Random
import qualified Data.Map as M

deathDrop :: String -> StdGen -> (Inv, StdGen)
deathDrop "Homunculus" = genDeathDrop [(wANDS, bound [0.6])]
deathDrop "Beetle" = genDeathDrop [(pOTIONS, bound [0.5])]
deathDrop "Bat" = genDeathDrop [(pOTIONS, bound [0.3, 0.8])]
deathDrop "Hunter" = genDeathDrop [(tRAPS, bound [0.3, 0.8]), (wEAPONS, bound [0.6])]
deathDrop "Ivy" = genDeathDrop [(sCROLLS, bound [0.9])]
deathDrop "Accelerator" = genDeathDrop [(sCROLLS, bound [0.6, 0.9])]
deathDrop "Troll" = genDeathDrop [(wANDS, bound [0.6])]
deathDrop "Worm" = genDeathDrop [([crysknife], bound [0.8])]
deathDrop "Floating eye" = genDeathDrop [(pOTIONS, bound [0.5])]
deathDrop _ = (\p -> (M.empty, p))

bound :: [Float] -> Float -> Int
bound list p = bound' list p 0 where
	bound' []     p n = n
	bound' (x:xs) p n = 
		if p < x
		then n
		else bound' xs p (n + 1)

genDeathDrop = genDeathDropByAlph notAlphabet

genDeathDropByAlph :: String -> [([Object], Float -> Int)] -> StdGen -> (Inv, StdGen)
genDeathDropByAlph _ [] g = (M.empty, g)
genDeathDropByAlph alph ((objs, f):xs) g =
	case f p of
		0 -> genDeathDropByAlph alph xs g'
		_ -> (M.insert (head alph) newObj rest, newG)
	where
		(p, g') = randomR (0.0, 1.0) g
		(ind, g'') = randomR (0, length objs - 1) g'
		newObj = (objs !! ind, f p)
		(rest, newG) = genDeathDropByAlph (tail alph) xs g''

pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation]
tRAPS = [bearTrap, fireTrap]
lAUNCHERS = [shortbow, bow, longbow]
wEAPONS = [dagger, shortsword, sword]
sCROLLS = [scrollOfFire, scrollOfAnimation, scrollOfCollection, scrollOfSafety, kabbalisticScroll]
wANDS =
	map wandOfStriking  [1..5] ++
	map wandOfStupidity [1..5] ++
	map wandOfSpeed     [1..2] ++
	map wandOfRadiation [1..4]
	
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

scrollOfSafety :: Object
scrollOfSafety = Scroll {
	title = "scroll of safety",
	actw = safety
}

kabbalisticScroll :: Object
kabbalisticScroll = Scroll {
	title = "Kabbalistic scroll",
	actw = spawnGolemsAround
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

wandOfSpeed :: Int -> Object
wandOfSpeed ch = Wand {
	title = "wand of speed",
	act = unrandom speed,
	range = 3,
	charge = ch
}

wandOfRadiation :: Int -> Object
wandOfRadiation ch = Wand {
	title = "wand of radiation",
	act = unrandom $ radiation 2,
	range = 5,
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
	objdmg = dices (1,12) 0.0 -- avg = 6.5
}

shortsword = Weapon {
	title = "shortsword",
	objdmg = dices (2,8) 0.1 -- avg = 8.1
}

sword = Weapon {
	title = "sword",
	objdmg = dices (2,10) 0.1 -- avg = 9.9
}

crysknife = Weapon {
	title = "crysknife",
	objdmg = dices (5,5) 0.0 -- avg = 15
}

trapFromTerrain :: Terrain -> Object
trapFromTerrain t
	| t == bEARTRAP = bearTrap
	| t == fIRETRAP = fireTrap
	| otherwise = error "unknown trap"
 
