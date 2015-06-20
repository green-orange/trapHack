module Stuff where

import Data
import Utils4stuff
import Random
import Monsters
import HealDamage
import GarbageCollector
import Parts
import Golem
import Utils4mon

import System.Random
import qualified Data.Map as M

deathDrop :: String -> StdGen -> (Inv, StdGen)
deathDrop "Homunculus" = genDeathDrop [(wANDS, bound [0.6])]
deathDrop "Beetle" = genDeathDrop [(pOTIONS, bound [0.5])]
deathDrop "Bat" = genDeathDrop [(pOTIONS, bound [0.3, 0.8])]
deathDrop "Hunter" = genDeathDrop [(tRAPS, bound [0.3, 0.8])]
deathDrop "Ivy" = genDeathDrop [(sCROLLS, bound [0.9])]
deathDrop "Accelerator" = genDeathDrop [(sCROLLS, bound [0.6, 0.9])]
deathDrop "Troll" = genDeathDrop [(wANDS, bound [0.6])]
deathDrop "Worm" = genDeathDrop [([crysknife], bound [0.8])]
deathDrop "Floating eye" = genDeathDrop [(pOTIONS, bound [0.5])]
deathDrop "Dragon" = genDeathDrop [(sCROLLS, bound [0.2, 0.4, 0.6, 0.8])]
deathDrop "Forgotten beast" = genDeathDrop [(sTACKABLE, bound inverseSquareList)]
deathDrop "Spider" = genDeathDrop [(wANDS, bound [0.4])]
deathDrop _ = (\p -> (M.empty, p))

bound :: [Float] -> Float -> Int
bound list' p = bound' list' p 0 where
	bound' []     _  n = n
	bound' (x:xs) p' n = 
		if p' < x
		then n
		else bound' xs p' (n + 1)

genDeathDrop :: [([Object], Float -> Int)] -> StdGen -> (Inv, StdGen)
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

sTACKABLE :: [Object]
sTACKABLE = pOTIONS ++ tRAPS ++ sCROLLS ++ mISSILES
	
potionOfHealing, potionOfIntellect, potionOfMutation :: Object

pOTIONS :: [Object]
pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation]

potionOfHealing = Potion {title = "potion of healing",
	act = unrandom $ healParts bODY 10}

potionOfIntellect = Potion {title = "potion of intellect",
	act = unrandom $ upgradeParts hEAD 5}

potionOfMutation = Potion {title = "potion of mutation",
	act = addRandomPart}

scrollOfFire, scrollOfAnimation, scrollOfCollection, scrollOfSafety, 
	kabbalisticScroll :: Object

sCROLLS :: [Object]
sCROLLS = [scrollOfFire, scrollOfAnimation, scrollOfCollection, 
	scrollOfSafety, kabbalisticScroll]

scrollOfFire = Scroll {title = "scroll of fire",
	actw = fireAround 1 (5, 10)}

scrollOfAnimation = Scroll {title = "scroll of animation",
	actw = animateAround}

scrollOfCollection = Scroll {title = "scroll of collection",
	actw = randomSpawn getGarbageCollector}

scrollOfSafety = Scroll {title = "scroll of safety",
	actw = safety}

kabbalisticScroll = Scroll {title = "Kabbalistic scroll",
	actw = spawnGolemsAround}

wandOfStriking, wandOfStupidity, wandOfSpeed, wandOfRadiation, 
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing :: Int -> Object

wANDS :: [Object]
wANDS =
	map wandOfStriking     [1..5] ++
	map wandOfStupidity    [1..5] ++
	map wandOfSpeed        [1..2] ++
	map wandOfRadiation    [1..3] ++
	map wandOfPsionicBlast [1..2] ++
	map wandOfSlowing      [1..5]

wandOfStriking ch = Wand {title = "wand of striking",
	act = unrandom $ dmgAll $ Just 10, range = 5, charge = ch}

wandOfStupidity ch = Wand {title = "wand of stupidity",
	act = unrandom stupidity, range = 3, charge = ch}

wandOfSpeed ch = Wand {title = "wand of speed",
	act = unrandom speed, range = 3, charge = ch}

wandOfRadiation ch = Wand {title = "wand of radiation",
	act = unrandom $ radiation 1, range = 5, charge = ch}

wandOfPsionicBlast ch = Wand {title = "wand of psionic blast",
	act = unrandom $ capture, range = 2, charge = ch}

wandOfPoison ch = Wand {title = "wand of poison",
	act = randPoison (1, 20), range = 5, charge = ch}

wandOfSlowing ch = Wand {title = "wand of slowing",
	act = unrandom $ slow, range = 5, charge = ch}

bearTrap, fireTrap, poisonTrap, magicTrap :: Object

tRAPS :: [Object]
tRAPS = [bearTrap, fireTrap, poisonTrap, magicTrap]

bearTrap = Trap {title = "bear trap", num = bEARTRAP}
fireTrap = Trap {title = "fire trap", num = fIRETRAP}
poisonTrap = Trap {title = "poison trap", num = pOISONTRAP}
magicTrap = Trap {title = "magic trap",	num = mAGICTRAP}

arrow :: Object

mISSILES :: [Object]
mISSILES = [arrow]

arrow = Missile {title = "arrow", objdmg = dices (1,6) 0.2,
	launcher = "bow"}

shortbow, bow, longbow :: Object

lAUNCHERS :: [Object]
lAUNCHERS = [shortbow, bow, longbow]

shortbow = Launcher {title = "short bow",
	count = 1, category = "bow"}

bow = Launcher {title = "bow",
	count = 2, category = "bow"}

longbow = Launcher {title = "longbow",
	count = 3, category = "bow"}

dagger, shortsword, sword, crysknife :: Object

wEAPONS :: [Object]
wEAPONS = 
	replicate 4 dagger ++
	replicate 2 shortsword ++
	replicate 1 sword

dagger = Weapon {title = "dagger",
	objdmg = dices (1,12) 0.0} -- avg = 6.5}

shortsword = Weapon {title = "shortsword",
	objdmg = dices (2,8) 0.1} -- avg = 8.1

sword = Weapon {title = "sword",
	objdmg = dices (2,10) 0.1} -- avg = 9.9

crysknife = Weapon {title = "crysknife",
	objdmg = dices (5,5) 0.0} -- avg = 15

aRMOR, bODYaRMOR, hELMETS, gLOVES, bOOTS :: [Object]
aRMOR = bODYaRMOR ++ hELMETS ++ gLOVES ++ bOOTS

leatherJacket, leatherArmor, ringMail, plateMail :: Object
bODYaRMOR = 
	replicate 4 leatherJacket ++
	replicate 3 leatherArmor ++ 
	replicate 2 ringMail ++
	replicate 1 plateMail

leatherJacket = Armor {title = "leather jacket", ac = 1, bind = bODY}
leatherArmor = Armor {title = "leather armor", ac = 2, bind = bODY}
ringMail = Armor {title = "ring mail", ac = 3, bind = bODY}
plateMail = Armor {title = "plate mail", ac = 4, bind = bODY}

fedora, hardHat, helmet, kabuto :: Object
hELMETS = 
	replicate 4 fedora ++
	replicate 3 hardHat ++ 
	replicate 2 helmet ++
	replicate 1 kabuto

fedora = Armor {title = "fedora", ac = 1, bind = hEAD}
hardHat = Armor {title = "hard hat", ac = 2, bind = hEAD}
helmet = Armor {title = "helmet", ac = 3, bind = hEAD}
kabuto = Armor {title = "kabuto", ac = 4, bind = hEAD}

glove, gauntlet :: Object
gLOVES = [glove, gauntlet]

glove = Armor {title = "glove", ac = 1, bind = aRM}
gauntlet = Armor {title = "gauntlet", ac = 2, bind = aRM}

lowBoot, highBoot :: Object
bOOTS = [lowBoot, highBoot]

lowBoot = Armor {title = "low boot", ac = 1, bind = lEG}
highBoot = Armor {title = "high boot", ac = 2, bind = lEG}

trapFromTerrain :: Terrain -> Object
trapFromTerrain t = tRAPS !! (t - 1)
 
