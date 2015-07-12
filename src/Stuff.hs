module Stuff where

import Data
import Utils4stuff
import Random
import Monsters
import HealDamage
import GarbageCollector
import Golem
import Utils4mon
import Changes
import DataDef

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
deathDrop "Red dragon" = dragonDrop
deathDrop "White dragon" = dragonDrop
deathDrop "Green dragon" = dragonDrop
deathDrop "Forgotten beast" = genDeathDrop [(sTACKABLE, bound inverseSquareList)]
deathDrop "Spider" = genDeathDrop [(jEWELRY, bound [0.6])]
deathDrop "Umber hulk" = genDeathDrop [(wANDS, bound [0.6])]
deathDrop _ = \p -> (M.empty, p)

dragonDrop :: StdGen -> (Inv, StdGen)
dragonDrop = genDeathDrop [(jEWELRY, bound [0.4])]

bound :: [Float] -> Float -> Int
bound list' p = bound' list' p 0 where
	bound' []     _  n = n
	bound' (x:xs) p' n = 
		if p' < x
		then n
		else bound' xs p' (n + 1)

genDeathDrop :: [([Object], Float -> Int)] -> StdGen -> (Inv, StdGen)
genDeathDrop = genDeathDropByAlph $ tail notAlphabet

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
	
potionOfHealing, potionOfIntellect, potionOfMutation, potionOfEnchantWeapon,
	potionOfEnchantArmor, potionOfEnchantJewelry :: Object

pOTIONS :: [Object]
pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation, 
	potionOfEnchantWeapon, potionOfEnchantArmor, potionOfEnchantJewelry]

potionOfHealing = Potion {title = "potion of healing",
	act = unrandom $ healParts bODY 10, idO = 0}

potionOfIntellect = Potion {title = "potion of intellect",
	act = unrandom $ upgradeParts hEAD 5, idO = 1}

potionOfMutation = Potion {title = "potion of mutation",
	act = addRandomPart, idO = 2}

potionOfEnchantWeapon = Potion {title = "potion of enchant weapon",
	act = unrandom $ enchantAll WeaponSlot 1, idO = 3}

potionOfEnchantArmor = Potion {title = "potion of enchant armor",
	act = unrandom $ enchantAll ArmorSlot 1, idO = 4}

potionOfEnchantJewelry = Potion {title = "potion of enchant jewelry",
	act = unrandom $ enchantAll JewelrySlot 1, idO = 5}

scrollOfFire, scrollOfAnimation, scrollOfCollection, scrollOfSafety, 
	kabbalisticScroll :: Object

sCROLLS :: [Object]
sCROLLS = [scrollOfFire, scrollOfAnimation, scrollOfCollection, 
	scrollOfSafety, kabbalisticScroll]

scrollOfFire = Scroll {title = "scroll of fire",
	actw = fireAround 1 (5, 10), idO = 0}

scrollOfAnimation = Scroll {title = "scroll of animation",
	actw = animateAround, idO = 1}

scrollOfCollection = Scroll {title = "scroll of collection",
	actw = randomSpawn getGarbageCollector, idO = 2}

scrollOfSafety = Scroll {title = "scroll of safety",
	actw = safety, idO = 3}

kabbalisticScroll = Scroll {title = "Kabbalistic scroll",
	actw = spawnGolemsAround, idO = 4}

wandOfStriking, wandOfStupidity, wandOfSpeed, wandOfRadiation, 
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing, wandOfStun
	:: Int -> Object

uNIQUEwANDS :: [Int -> Object]
uNIQUEwANDS = [wandOfStriking, wandOfStupidity, wandOfSpeed, wandOfRadiation,
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing, wandOfStun]
wANDS :: [Object]
wANDS =
	map wandOfStriking     [1..5] ++
	map wandOfStupidity    [1..5] ++
	map wandOfSpeed        [1..2] ++
	map wandOfRadiation    [1..3] ++
	map wandOfPsionicBlast [1..2] ++
	map wandOfPoison       [1..3] ++
	map wandOfSlowing      [1..5] ++
	map wandOfStun         [1..4]

wandOfStriking ch = Wand {title = "wand of striking",
	act = unrandom $ dmgAll $ Just 10, range = 5, charge = ch, idO = 0}

wandOfStupidity ch = Wand {title = "wand of stupidity",
	act = unrandom stupidity, range = 3, charge = ch, idO = 1}

wandOfSpeed ch = Wand {title = "wand of speed",
	act = unrandom $ speed 10, range = 3, charge = ch, idO = 2}

wandOfRadiation ch = Wand {title = "wand of radiation",
	act = unrandom $ radiation 1, range = 5, charge = ch, idO = 3}

wandOfPsionicBlast ch = Wand {title = "wand of psionic blast",
	act = unrandom capture, range = 2, charge = ch, idO = 4}

wandOfPoison ch = Wand {title = "wand of poison",
	act = randTemp Poison (1, 20), range = 5, charge = ch, idO = 5}

wandOfSlowing ch = Wand {title = "wand of slowing",
	act = unrandom $ speed (-10), range = 5, charge = ch, idO = 6}

wandOfStun ch = Wand {title = "wand of stun", act = randTemp Stun (1, 10),
	range = 3, charge = ch, idO = 7}

bearTrap, fireTrap, poisonTrap, magicTrap :: Object

tRAPS :: [Object]
tRAPS = [bearTrap, fireTrap, poisonTrap, magicTrap]

bearTrap = Trap {title = "bear trap", num = BearTrap, idO = 0}
fireTrap = Trap {title = "fire trap", num = FireTrap, idO = 1}
poisonTrap = Trap {title = "poison trap", num = PoisonTrap, idO = 2}
magicTrap = Trap {title = "magic trap",	num = MagicTrap, idO = 3}

arrow :: Object

mISSILES :: [Object]
mISSILES = [arrow]

getMissile :: String -> StdDmg -> String -> Object
getMissile t o l = Missile {title = t, objdmg' = o, launcher = l,
	enchantment = 0, idO = 0}

arrow = getMissile "arrow" (dices (1,6) 0.2) "bow"

shortbow, bow, longbow :: Object

lAUNCHERS :: [Object]
lAUNCHERS = [shortbow, bow, longbow]

getLauncher :: String -> Int -> Int -> String -> Int -> Object
getLauncher t w c cat id' = Launcher {title = t, count' = c, category = cat, 
	enchantment = 0, weight' = w, idO = id'}

shortbow = getLauncher "short bow" 20 1 "bow" 0

bow = getLauncher "bow" 30 2 "bow" 1

longbow = getLauncher "longbow" 40 3 "bow" 2

dagger, shortsword, sword, crysknife :: Object

uNIQUEwEAPONS :: [Object]
uNIQUEwEAPONS = [dagger, shortsword, sword, crysknife]
wEAPONS :: [Object]
wEAPONS = 
	replicate 4 dagger ++
	replicate 2 shortsword ++
	replicate 1 sword

getWeapon :: String -> Int -> Int -> StdDmg -> Object
getWeapon t w id' o = Weapon {title = t, objdmg' = o, enchantment = 0, 
	weight' = w, idO = id'}

dagger = getWeapon "dagger" 10 0 $ dices (1,12) 0.0 -- avg = 6.5

shortsword = getWeapon "shortsword" 30 1 $ dices (2,8) 0.1 -- avg = 8.1

sword = getWeapon "sword" 40 2 $ dices (2,10) 0.1 -- avg = 9.9

crysknife = getWeapon "crysknife" 20 3 $ dices (5,5) 0.0-- avg = 15

uNIQUEaRMOR, uNIQUEhELMS, uNIQUEgLOVES, uNIQUEbOOTS :: [Object]
aRMOR, bODYaRMOR, hELMETS, gLOVES, bOOTS :: [Object]
aRMOR = bODYaRMOR ++ hELMETS ++ gLOVES ++ bOOTS

getArmor :: String -> Int -> Int -> Int -> Int -> Object
getArmor t w a b id' = Armor {title = t, ac' = a, bind = b, enchantment = 0, 
	weight' = w, idO = id'}

uNIQUEaRMOR = [leatherJacket, leatherArmor, ringMail, plateMail]
leatherJacket, leatherArmor, ringMail, plateMail :: Object
bODYaRMOR = 
	replicate 4 leatherJacket ++
	replicate 3 leatherArmor ++ 
	replicate 2 ringMail ++
	replicate 1 plateMail

leatherJacket = getArmor "leather jacket" 30 1 bODY 0
leatherArmor = getArmor "leather armor" 150 2 bODY 1
ringMail = getArmor "ring mail" 250 3 bODY 2
plateMail = getArmor "plate mail" 450 4 bODY 3

uNIQUEhELMS = [fedora, hardHat, helmet, kabuto]
fedora, hardHat, helmet, kabuto :: Object
hELMETS =
	replicate 4 fedora ++
	replicate 3 hardHat ++
	replicate 2 helmet ++
	replicate 1 kabuto

fedora = getArmor "fedora" 3 1 hEAD 0
hardHat = getArmor "hard hat" 20 2 hEAD 1
helmet = getArmor "helmet" 40 3 hEAD 2
kabuto = getArmor "kabuto" 50 4 hEAD 3

uNIQUEgLOVES = gLOVES
glove, gauntlet :: Object
gLOVES = [glove, gauntlet]

glove = getArmor "glove" 3 1 aRM 0
gauntlet = getArmor "gauntlet" 5 2 aRM 1

uNIQUEbOOTS = bOOTS
lowBoot, highBoot :: Object
bOOTS = [lowBoot, highBoot]

lowBoot = getArmor "low boot" 10 1 lEG 0
highBoot = getArmor "high boot" 20 2 lEG 1

uNIQUErINGS, uNIQUEaMULETS :: [Int -> Object]
jEWELRY, rINGS, aMULETS :: [Object]
jEWELRY = rINGS ++ aMULETS

uNIQUErINGS = [ringOfSpeed, ringOfFireRes, ringOfColdRes, 
	ringOfPoisonRes, ringOfProtection]
ringOfSpeed, ringOfFireRes, ringOfColdRes, ringOfPoisonRes, ringOfProtection 
	:: Int -> Object
rINGS = 
	map ringOfSpeed      [1..4] ++
	map ringOfFireRes    [1..3] ++
	map ringOfColdRes    [1..3] ++
	map ringOfPoisonRes  [1..3] ++
	map ringOfProtection [1..3]

ringOfSpeed ench = Jewelry {title = "ring of speed", enchantment = ench,
	bind = aRM, effectOn = \ench' -> speed (5 * ench'), 
	effectOff = \ench' -> speed (-5 * ench'), idO = 0}

getRingRes :: String -> Elem -> Int -> Int -> Object
getRingRes title' elem' id' ench = Jewelry {title = title', enchantment = ench,
	bind = aRM, effectOn = \ench' -> addRes elem' (2 * ench'), 
	effectOff = \ench' -> addRes elem' (-2 * ench'), idO = id'}

ringOfFireRes = getRingRes "ring of fire resistance" Fire 1
ringOfColdRes = getRingRes "ring of cold resistance" Cold 2
ringOfPoisonRes = getRingRes "ring of poison resistance" Poison' 3

ringOfProtection ench = Jewelry {title = "ring of protection", enchantment = ench,
	bind = aRM, effectOn = flip const, effectOff = flip const, idO = 4}

uNIQUEaMULETS = [amuletOfTeleportation]
amuletOfTeleportation :: Int -> Object
aMULETS = 
	map amuletOfTeleportation [1..6]

getIntrAmulet :: Int -> String -> Intr -> Int -> Int -> Object
getIntrAmulet mult title' intr' id' ench = Jewelry {title = title', enchantment = ench,
	bind = hEAD, effectOn = \ench' -> addIntr intr' (mult * ench'), 
	effectOff = \ench' -> addIntr intr' (-mult * ench'), idO = id'}

amuletOfTeleportation = getIntrAmulet 2 "amulet of teleportation" Teleport 0

trapFromTerrain :: Terrain -> Object
trapFromTerrain x = case x of
	BearTrap -> bearTrap
	FireTrap -> fireTrap
	PoisonTrap -> poisonTrap
	MagicTrap -> magicTrap
	_ -> error "unknown trap"

foodRation :: Object
foodRation = Food {
	title = "food ration",
	nutrition = 200,
	weight' = 20
}
