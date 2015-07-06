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
import Changes

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
	
potionOfHealing, potionOfIntellect, potionOfMutation, potionOfEnchantWeapon,
	potionOfEnchantArmor, potionOfEnchantJewelry :: Object

pOTIONS :: [Object]
pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation, 
	potionOfEnchantWeapon, potionOfEnchantArmor, potionOfEnchantJewelry]

potionOfHealing = Potion {title = "potion of healing",
	act = unrandom $ healParts bODY 10}

potionOfIntellect = Potion {title = "potion of intellect",
	act = unrandom $ upgradeParts hEAD 5}

potionOfMutation = Potion {title = "potion of mutation",
	act = addRandomPart}

potionOfEnchantWeapon = Potion {title = "potion of enchant weapon",
	act = unrandom $ enchantAll WeaponSlot 1}

potionOfEnchantArmor = Potion {title = "potion of enchant armor",
	act = unrandom $ enchantAll ArmorSlot 1}

potionOfEnchantJewelry = Potion {title = "potion of enchant jewelry",
	act = unrandom $ enchantAll JewelrySlot 1}

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
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing, wandOfStun 
	:: Int -> Object

wANDS :: [Object]
wANDS =
	map wandOfStriking     [1..5] ++
	map wandOfStupidity    [1..5] ++
	map wandOfSpeed        [1..2] ++
	map wandOfRadiation    [1..3] ++
	map wandOfPsionicBlast [1..2] ++
	map wandOfSlowing      [1..5] ++
	map wandOfStun         [1..4]

wandOfStriking ch = Wand {title = "wand of striking",
	act = unrandom $ dmgAll $ Just 10, range = 5, charge = ch}

wandOfStupidity ch = Wand {title = "wand of stupidity",
	act = unrandom stupidity, range = 3, charge = ch}

wandOfSpeed ch = Wand {title = "wand of speed",
	act = unrandom $ speed 10, range = 3, charge = ch}

wandOfRadiation ch = Wand {title = "wand of radiation",
	act = unrandom $ radiation 1, range = 5, charge = ch}

wandOfPsionicBlast ch = Wand {title = "wand of psionic blast",
	act = unrandom capture, range = 2, charge = ch}

wandOfPoison ch = Wand {title = "wand of poison",
	act = randTemp Poison (1, 20), range = 5, charge = ch}

wandOfSlowing ch = Wand {title = "wand of slowing",
	act = unrandom $ speed (-10), range = 5, charge = ch}

wandOfStun ch = Wand {title = "wand of stun", act = randTemp Stun (1, 10),
	range = 3, charge = ch}

bearTrap, fireTrap, poisonTrap, magicTrap :: Object

tRAPS :: [Object]
tRAPS = [bearTrap, fireTrap, poisonTrap, magicTrap]

bearTrap = Trap {title = "bear trap", num = BearTrap}
fireTrap = Trap {title = "fire trap", num = FireTrap}
poisonTrap = Trap {title = "poison trap", num = PoisonTrap}
magicTrap = Trap {title = "magic trap",	num = MagicTrap}

arrow :: Object

mISSILES :: [Object]
mISSILES = [arrow]

getMissile :: String -> StdDmg -> String -> Object
getMissile t o l = Missile {title = t, objdmg' = o, launcher = l,
	enchantment = 0}

arrow = getMissile "arrow" (dices (1,6) 0.2) "bow"

shortbow, bow, longbow :: Object

lAUNCHERS :: [Object]
lAUNCHERS = [shortbow, bow, longbow]

getLauncher :: String -> Int -> String -> Object
getLauncher t c cat = Launcher {title = t, count' = c, category = cat, 
	enchantment = 0}

shortbow = getLauncher "short bow" 1 "bow"

bow = getLauncher "bow" 2 "bow"

longbow = getLauncher "longbow" 3 "bow"

dagger, shortsword, sword, crysknife :: Object

wEAPONS :: [Object]
wEAPONS = 
	replicate 4 dagger ++
	replicate 2 shortsword ++
	replicate 1 sword

getWeapon :: String -> StdDmg -> Object
getWeapon t o = Weapon {title = t, objdmg' = o, enchantment = 0}

dagger = getWeapon "dagger" $ dices (1,12) 0.0 -- avg = 6.5

shortsword = getWeapon "shortsword" $ dices (2,8) 0.1 -- avg = 8.1

sword = getWeapon "sword" $ dices (2,10) 0.1 -- avg = 9.9

crysknife = getWeapon "crysknife" $ dices (5,5) 0.0 -- avg = 15

aRMOR, bODYaRMOR, hELMETS, gLOVES, bOOTS :: [Object]
aRMOR = bODYaRMOR ++ hELMETS ++ gLOVES ++ bOOTS

getArmor :: String -> Int -> Int -> Object
getArmor t a b = Armor {title = t, ac' = a, bind = b, enchantment = 0}

leatherJacket, leatherArmor, ringMail, plateMail :: Object
bODYaRMOR = 
	replicate 4 leatherJacket ++
	replicate 3 leatherArmor ++ 
	replicate 2 ringMail ++
	replicate 1 plateMail

leatherJacket = getArmor "leather jacket" 1 bODY
leatherArmor = getArmor "leather armor" 2 bODY
ringMail = getArmor "ring mail" 3 bODY
plateMail = getArmor "plate mail" 4 bODY

fedora, hardHat, helmet, kabuto :: Object
hELMETS =
	replicate 4 fedora ++
	replicate 3 hardHat ++ 
	replicate 2 helmet ++
	replicate 1 kabuto

fedora = getArmor "fedora" 1 hEAD
hardHat = getArmor "hard hat" 2 hEAD
helmet = getArmor "helmet" 3 hEAD
kabuto = getArmor "kabuto" 4 hEAD

glove, gauntlet :: Object
gLOVES = [glove, gauntlet]

glove = getArmor "glove" 1 aRM
gauntlet = getArmor "gauntlet" 2 aRM

lowBoot, highBoot :: Object
bOOTS = [lowBoot, highBoot]

lowBoot = getArmor "low boot" 1 lEG
highBoot = getArmor "high boot" 2 lEG

jEWELRY, rINGS, aMULETS :: [Object]
jEWELRY = rINGS ++ aMULETS

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
	effectOff = \ench' -> speed (-5 * ench')}

getRingRes :: String -> Elem -> Int -> Object
getRingRes title' elem' ench = Jewelry {title = title', enchantment = ench,
	bind = aRM, effectOn = \ench' -> addRes elem' (2 * ench'), 
	effectOff = \ench' -> addRes elem' (-2 * ench')}

ringOfFireRes = getRingRes "ring of fire resistance" Fire
ringOfColdRes = getRingRes "ring of cold resistance" Cold
ringOfPoisonRes = getRingRes "ring of poison resistance" Poison'

ringOfProtection ench = Jewelry {title = "ring of protection", enchantment = ench,
	bind = aRM, effectOn = flip const, effectOff = flip const}

amuletOfTeleportation :: Int -> Object
aMULETS = 
	map amuletOfTeleportation [1..6]

getIntrAmulet :: Int -> String -> Intr -> Int -> Object
getIntrAmulet mult title' intr' ench = Jewelry {title = title', enchantment = ench,
	bind = hEAD, effectOn = \ench' -> addIntr intr' (mult * ench'), 
	effectOff = \ench' -> addIntr intr' (-mult * ench')}

amuletOfTeleportation = getIntrAmulet 5 "amulet of teleportation" Teleport

trapFromTerrain :: Terrain -> Object
trapFromTerrain x = case x of
	BearTrap -> bearTrap
	FireTrap -> fireTrap
	PoisonTrap -> poisonTrap
	MagicTrap -> magicTrap
	_ -> error "unknown trap"
 
