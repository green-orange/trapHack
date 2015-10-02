module Items.Stuff where

import Data.Const
import Data.Define
import Data.ID
import Utils.Stuff
import Utils.Random
import Utils.HealDamage
import Utils.Monsters
import Utils.Changes
import Utils.Items
import Monsters.Monsters
import IO.Texts

import System.Random
import qualified Data.Map as M
import Data.Functor ((<$>))

-- | homunculus sometmes generates with attack wand
wandForHom :: Float -> Int -> Object
wandForHom = flip uniformFromList [wandOfPoison, wandOfSlowing, 
	wandOfSpeed, wandOfStriking, wandOfStun]

-- | generate an inventory for a homunculus
invHom :: StdGen -> (Inv, StdGen)
invHom g =
	(if p <= 0.1
	then M.singleton 'a' (wandForHom q 1, 1)
	else M.empty, g'') where
		p, q :: Float
		(p, g') = randomR (0.0, 1.0) g 
		(q, g'') = randomR (0.0, 1.0) g'

-- | hunter always generates with a bow
hunterInv :: InvGen
hunterInv g = (M.fromList $ zip alphabet $ addRation
	[(arrow, 10 * inverseSquareRandom p)
	,(lAUNCHERS !! uniform q 0 (length lAUNCHERS - 1), 1)
	], g3) where
		(p, g1) = randomR (0.0, 1.0) g
		(q, g2) = randomR (0.0, 1.0) g1
		(n, g3) = randomR (0, 2) g2
		addRation = if n == 0 then id else (:) (foodRation, n)

-- | soldier generates with weapon, different armor and some food 
soldierInv :: InvGen
soldierInv g = (M.fromList $ zip alphabet $ addRation $ zip
	[uniformFromList x1 wEAPONS,
	uniformFromList x2 aRMOR,
	uniformFromList x3 aRMOR] [1,1..], g4) where
		(x1, g1) = randomR (0.0, 1.0) g
		(x2, g2) = randomR (0.0, 1.0) g1
		(x3, g3) = randomR (0.0, 1.0) g2
		(n,  g4) = randomR (0, 2) g3
		addRation = if n == 0 then id else (:) (foodRation, n)

-- | generate death drop by monster name
deathDrop :: Int -> StdGen -> (Inv, StdGen)
deathDrop x
	| x == idHom = genDeathDrop [(wANDS, bound [0.6])]
	| x == idBtl = genDeathDrop [(pOTIONS, bound [0.5])]
	| x == idBat = genDeathDrop [(pOTIONS, bound [0.3, 0.8])]
	| x == idHun = genDeathDrop [(tRAPS, bound [0.3, 0.8])]
	| x == idIvy = genDeathDrop [(sCROLLS, bound [0.9])]
	| x == idAcc = genDeathDrop [(sCROLLS, bound [0.6, 0.9])]
	| x == idTrl = genDeathDrop [(wANDS, bound [0.6])]
	| x == idWrm = genDeathDrop [([crysknife], bound [0.8])]
	| x == idFlE = genDeathDrop [(pOTIONS, bound [0.5])]
	| x == idRDr || x == idWDr || x == idGDr = dragonDrop
	| x == idFgB = genDeathDrop [(sTACKABLE, bound inverseSquareList)]
	| x == idSpd = genDeathDrop [(jEWELRY, bound [0.6])]
	| x == idUmH = genDeathDrop [(wANDS, bound [0.6])]
	| x == idTre = genDeathDrop [([itemFromRes Tree], 3 * bound inverseSquareList)]
	| x == idBot = genDeathDrop [([itemFromRes MetalScrap], bound [0.7])]
	| x == idBsh = genDeathDrop [(bERRIES, bound [0.2, 0.5, 0.9])]
	| otherwise = \p -> (M.empty, p)

-- | generate drop for all dragons
dragonDrop :: StdGen -> (Inv, StdGen)
dragonDrop = genDeathDrop [(jEWELRY, bound [0.4])]

-- | return last n such that sum of first n element of the list is less then q
bound :: [Float] -> Float -> Int
bound list' p = bound' list' p 0 where
	bound' []     _  n = n
	bound' (x:xs) p' n = 
		if p' < x
		then n
		else bound' xs p' (n + 1)

-- | converts list [object_category, cnt_func] to the inventory
genDeathDrop :: [([Object], Float -> Int)] -> StdGen -> (Inv, StdGen)
genDeathDrop = genDeathDropByAlph $ tail notAlphabet

-- | converts list [object_category, cnt_func] to the inventory
-- with given alphabet
genDeathDropByAlph :: String -> [([Object], Float -> Int)] -> StdGen -> (Inv, StdGen)
genDeathDropByAlph _ [] g = (M.empty, g)
genDeathDropByAlph [] _ _ = putWE "genDeathDropByAlph"
genDeathDropByAlph alph@(a:as) ((objs, f):xs) g =
	case f p of
		0 -> genDeathDropByAlph alph xs g'
		_ -> (M.insert a newObj rest, newG)
	where
		(p, g') = randomR (0.0, 1.0) g
		(ind, g'') = randomR (0, length objs - 1) g'
		newObj = (objs !! ind, f p)
		(rest, newG) = genDeathDropByAlph as xs g''

-- | list of all stackable items
sTACKABLE :: [Object]
sTACKABLE = pOTIONS ++ tRAPS ++ sCROLLS ++ mISSILES
	
potionOfHealing, potionOfIntellect, potionOfMutation, potionOfEnchantWeapon,
	potionOfEnchantArmor, potionOfEnchantJewelry, soup :: Object
-- | list of all potions
pOTIONS :: [Object]
pOTIONS = [potionOfHealing, potionOfIntellect, potionOfMutation, 
	potionOfEnchantWeapon, potionOfEnchantArmor, potionOfEnchantJewelry,
	soup]
-- | heals your body
potionOfHealing = Potion {title = "potion of healing",
	act = addRandom (5, 15) $ healParts bODY, idO = 0}
-- | increase current and maximal hp of your head and
-- it regeneration rate
potionOfIntellect = Potion {title = "potion of intellect",
	act = addRandom (1, 10) $ upgradeParts hEAD, idO = 1}
-- | add random body part
potionOfMutation = Potion {title = "potion of mutation",
	act = addRandomPart, idO = 2}
-- | enchant all your wielded weapons
potionOfEnchantWeapon = Potion {title = "potion of enchant weapon",
	act = addRandom (0, 3) $ enchantAll WeaponSlot, idO = 3}
-- | enchant all your weared armor
potionOfEnchantArmor = Potion {title = "potion of enchant armor",
	act = addRandom (0, 3) $ enchantAll ArmorSlot, idO = 4}
-- | enchant all your putted jewelry
potionOfEnchantJewelry = Potion {title = "potion of enchant jewelry",
	act = addRandom (0, 3) $ enchantAll JewelrySlot, idO = 5}
-- | just increase your satiety
soup = Potion {title = "soup", act = addRandom (20, 100) addNutr, idO = 6}

scrollOfFire, scrollOfAnimation, scrollOfCollection, scrollOfSafety, 
	kabbalisticScroll, scrollOfBandaging :: Object
-- | list of all scrolls
sCROLLS :: [Object]
sCROLLS = [scrollOfFire, scrollOfAnimation, scrollOfCollection, 
	scrollOfSafety, kabbalisticScroll, scrollOfBandaging]
-- | attacks all mosnters near you with fire; creates some bonfires
scrollOfFire = Scroll {title = "scroll of fire",
	actw = fireAround 1 (5, 10), idO = 0}
-- | creates dummies on the near cells;
-- their hp equals to number of items on this cell
scrollOfAnimation = Scroll {title = "scroll of animation",
	actw = animateAround, idO = 1}
-- | create a garbage collector near you
scrollOfCollection = Scroll {title = "scroll of collection",
	actw = randomSpawn getGarbageCollector, idO = 2}
-- | teleports you to the new world
scrollOfSafety = Scroll {title = "scroll of safety",
	actw = safety, idO = 3}
-- | creates some golems from items near you
kabbalisticScroll = Scroll {title = "Kabbalistic scroll",
	actw = spawnGolemsAround, idO = 4}
-- | heal all your limbs
scrollOfBandaging = Scroll {title = "scroll of bandaging",
	actw = actWithFirst $ addRandom (1, 10) healLimbs, idO = 5}

wandOfStriking, wandOfStupidity, wandOfSpeed, wandOfRadiation, 
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing, wandOfStun
	:: Int -> Object
-- | list of all function (charge -> wand)
uNIQUEwANDS :: [Int -> Object]
uNIQUEwANDS = [wandOfStriking, wandOfStupidity, wandOfSpeed, wandOfRadiation,
	wandOfPsionicBlast, wandOfPoison, wandOfSlowing, wandOfStun]
-- | list of charged wands to generate death drop 
wANDS :: [Object]
wANDS =
	(wandOfStriking     <$> [1..5]) ++
	(wandOfStupidity    <$> [1..5]) ++
	(wandOfSpeed        <$> [1..2]) ++
	(wandOfRadiation    <$> [1..3]) ++
	(wandOfPsionicBlast <$> [1..2]) ++
	(wandOfPoison       <$> [1..3]) ++
	(wandOfSlowing      <$> [1..5]) ++
	(wandOfStun         <$> [1..4])

instance Random a => Random (Maybe a) where
	random g = (Just x, g')
		where (x, g') = random g
	randomR (_, Nothing) g = (Nothing, g)
	randomR (Nothing, _) g = (Nothing, g)
	randomR (Just l, Just r) g = (Just x, g') 
		where (x, g') = randomR (l, r) g
-- | strikes all monsters on a line
wandOfStriking ch = Wand {title = "wand of striking",
	act = addRandom (Just 1, Just 20) dmgAll, range = 5, charge = ch, idO = 0}
-- | gives 'stupidestAI' to a monster if it can walk
wandOfStupidity ch = Wand {title = "wand of stupidity",
	act = unrandom stupidity, range = 3, charge = ch, idO = 1}
-- | increase speed
wandOfSpeed ch = Wand {title = "wand of speed",
	act = addRandom (1, 20) speed, range = 3, charge = ch, idO = 2}
-- | decrease regeneration rate
wandOfRadiation ch = Wand {title = "wand of radiation",
	act = addRandom (0, 2) radiation, range = 5, charge = ch, idO = 3}
-- | gives you control over the monster
wandOfPsionicBlast ch = Wand {title = "wand of psionic blast",
	act = unrandom capture, range = 2, charge = ch, idO = 4}
-- | temporary stop regeneration
wandOfPoison ch = Wand {title = "wand of poison",
	act = randTemp Poison (1, 20), range = 5, charge = ch, idO = 5}
-- | decrease speed
wandOfSlowing ch = Wand {title = "wand of slowing",
	act = addRandom (-20, -1) speed, range = 5, charge = ch, idO = 6}
-- | temporary randomize all moves
wandOfStun ch = Wand {title = "wand of stun", act = randTemp Stun (1, 10),
	range = 3, charge = ch, idO = 7}

bearTrap, fireTrap, poisonTrap, magicTrap :: Object
-- | list of all traps
tRAPS :: [Object]
tRAPS = [bearTrap, fireTrap, poisonTrap, magicTrap]
-- | block all moves (don't work with you and fluing creatures)
bearTrap = Trap {title = "bear trap", num = BearTrap, idO = 0}
-- | attack monster with fire
fireTrap = Trap {title = "fire trap", num = FireTrap, idO = 1}
-- | poison a monster
poisonTrap = Trap {title = "poison trap", num = PoisonTrap, idO = 2}
-- | apply random wand effect
magicTrap = Trap {title = "magic trap",	num = MagicTrap, idO = 3}

arrow :: Object
-- | list of all missiles
mISSILES :: [Object]
mISSILES = [arrow]
-- | get a missile by given title and dmg, launcher kind
getMissile :: String -> StdDmg -> String -> Object
getMissile t o l = Missile {title = t, objdmg' = o, launcher = l,
	enchantment = 0, idO = 0}
-- | standard missile for bows
arrow = getMissile "arrow" (dices (1,6) 0.2) "bow"

shortbow, bow, longbow :: Object

lAUNCHERS :: [Object]
lAUNCHERS = [shortbow, bow, longbow]
-- | get a launcher by title, weight, count of missiles by one shot,
-- category and id
getLauncher :: String -> Int -> Int -> String -> Int -> Object
getLauncher t w c cat id' = Launcher {title = t, count' = c, category = cat, 
	enchantment = 0, weight' = w, idO = id'}
-- | a short bow: one arrow by a shot
shortbow = getLauncher "short bow" 20 1 "bow" 0
-- | a bow: two arrows by a shot
bow = getLauncher "bow" 30 2 "bow" 1
-- | long bow: three arrow by a shot
longbow = getLauncher "longbow" 40 3 "bow" 2

dagger, shortsword, sword, crysknife, woodenSword, stoneSword, crowbar :: Object
-- | list of all weapons
uNIQUEwEAPONS :: [Object]
uNIQUEwEAPONS = [dagger, shortsword, sword, crysknife, woodenSword, 
	stoneSword, crowbar]
-- | list of all random generated weapons
wEAPONS :: [Object]
wEAPONS = 
	replicate 4 dagger ++
	replicate 2 shortsword ++
	replicate 1 sword
-- | get weapon by title, weight id and dmg
getWeapon :: String -> Int -> Int -> StdDmg -> Object
getWeapon t w id' o = Weapon {title = t, objdmg' = o, enchantment = 0, 
	weight' = w, idO = id'}
-- | dagger: average damage = 6.5
dagger      = getWeapon "dagger"       10 0 $ dices (1,12) 0.0
-- | short sword: average damage = 8.1
shortsword  = getWeapon "shortsword"   30 1 $ dices (2, 8) 0.1 
-- | sword: average damage = 9.9
sword       = getWeapon "sword"        40 2 $ dices (2,10) 0.1
-- | crysknife: average damage = 15.0
crysknife   = getWeapon "crysknife"    20 3 $ dices (5, 5) 0.0
-- | wooden sword: average damage = 5.6
woodenSword = getWeapon "wooden sword" 20 4 $ dices (2, 7) 0.3
-- | stone sword: average damage = 8.0
stoneSword  = getWeapon "stone sword"  50 5 $ dices (2, 9) 0.2
-- | crowbar: average damage = 9.0
crowbar     = getWeapon "crowbar"      30 6 $ dices (2, 8) 0.0

uNIQUEaRMOR, uNIQUEhELMS, uNIQUEgLOVES, uNIQUEbOOTS :: [Object]
aRMOR, bODYaRMOR, hELMETS, gLOVES, bOOTS :: [Object]
-- | list of all armor
aRMOR = bODYaRMOR ++ hELMETS ++ gLOVES ++ bOOTS
-- | get armor by title, weight, ac, binding (a body part) and id
getArmor :: String -> Int -> Int -> Int -> Int -> Object
getArmor t w a b id' = Armor {title = t, ac' = a, bind = b, enchantment = 0, 
	weight' = w, idO = id'}
-- | list of all body armor classes
uNIQUEaRMOR = [leatherJacket, leatherArmor, ringMail, plateMail]
leatherJacket, leatherArmor, ringMail, plateMail :: Object
-- | list of armor with coefficients to generate random armor
bODYaRMOR = 
	replicate 4 leatherJacket ++
	replicate 3 leatherArmor ++ 
	replicate 2 ringMail ++
	replicate 1 plateMail

leatherJacket = getArmor "leather jacket" 30 1 bODY 0
leatherArmor = getArmor "leather armor" 150 2 bODY 1
ringMail = getArmor "ring mail" 250 3 bODY 2
plateMail = getArmor "plate mail" 450 4 bODY 3
-- | list of all helms
uNIQUEhELMS = [fedora, hardHat, helmet, kabuto]
fedora, hardHat, helmet, kabuto :: Object
-- | list of helms with probabilities to generate random armor
hELMETS =
	replicate 4 fedora ++
	replicate 3 hardHat ++
	replicate 2 helmet ++
	replicate 1 kabuto

fedora = getArmor "fedora" 3 1 hEAD 0
hardHat = getArmor "hard hat" 20 2 hEAD 1
helmet = getArmor "helmet" 40 3 hEAD 2
kabuto = getArmor "kabuto" 50 4 hEAD 3
-- | list of all gloves
uNIQUEgLOVES = gLOVES
glove, gauntlet :: Object
-- | list of gloves with probabilities to generate random armor
gLOVES = [glove, gauntlet]

glove = getArmor "glove" 3 1 aRM 0
gauntlet = getArmor "gauntlet" 5 2 aRM 1
-- | list of all boots
uNIQUEbOOTS = bOOTS
lowBoot, highBoot :: Object
-- | list of boots with probablilities to generate random armor
bOOTS = [lowBoot, highBoot]

lowBoot = getArmor "low boot" 10 1 lEG 0
highBoot = getArmor "high boot" 20 2 lEG 1

uNIQUErINGS, uNIQUEaMULETS :: [Int -> Object]
jEWELRY, rINGS, aMULETS :: [Object]
-- | list of all jewelry
jEWELRY = rINGS ++ aMULETS
-- | list of all (enchantment -> ring) functions
uNIQUErINGS = [ringOfSpeed, ringOfFireRes, ringOfColdRes, 
	ringOfPoisonRes, ringOfProtection]
ringOfSpeed, ringOfFireRes, ringOfColdRes, ringOfPoisonRes, ringOfProtection 
	:: Int -> Object
-- | list of rings with probabilities to generate random jewelry
rINGS = 
	(ringOfSpeed      <$> [1..4]) ++
	(ringOfFireRes    <$> [1..3]) ++
	(ringOfColdRes    <$> [1..3]) ++
	(ringOfPoisonRes  <$> [1..3]) ++
	(ringOfProtection <$> [1..3])
-- | increase speed
ringOfSpeed ench = Jewelry {title = "ring of speed", enchantment = ench,
	bind = aRM, effectOn = \ench' -> speed (5 * ench'), 
	effectOff = \ench' -> speed (-5 * ench'), idO = 0}
-- | get a ring that gives you some resistance by title, element of resistance,
-- id and enchantment
getRingRes :: String -> Elem -> Int -> Int -> Object
getRingRes title' elem' id' ench = Jewelry {title = title', enchantment = ench,
	bind = aRM, effectOn = \ench' -> addRes elem' (2 * ench'), 
	effectOff = \ench' -> addRes elem' (-2 * ench'), idO = id'}

ringOfFireRes = getRingRes "ring of fire resistance" Fire 1
ringOfColdRes = getRingRes "ring of cold resistance" Cold 2
ringOfPoisonRes = getRingRes "ring of poison resistance" Poison' 3
-- | increase ac of the arm
ringOfProtection ench = Jewelry {title = "ring of protection", enchantment = ench,
	bind = aRM, effectOn = flip const, effectOff = flip const, idO = 4}
-- | list of all amulets
uNIQUEaMULETS = [amuletOfTeleportation]
amuletOfTeleportation :: Int -> Object
-- | list of amulets with probabilities to generate random jewelry
aMULETS = 
	amuletOfTeleportation <$> [1..6]
-- | get amulet that gives you some intrincic by multiplicator, title, 
-- intrinsic, id and enchantment
getIntrAmulet :: Int -> String -> Intr -> Int -> Int -> Object
getIntrAmulet mult title' intr' id' ench = Jewelry {title = title', enchantment = ench,
	bind = hEAD, effectOn = \ench' -> addIntr intr' (mult * ench'), 
	effectOff = \ench' -> addIntr intr' (-mult * ench'), idO = id'}
-- | gives you teleportation ability
amuletOfTeleportation = getIntrAmulet 2 "amulet of teleportation" Teleport 0
-- | converts terrain to a trap item if it's possible
trapFromTerrain :: Terrain -> Object
trapFromTerrain x = case x of
	BearTrap -> bearTrap
	FireTrap -> fireTrap
	PoisonTrap -> poisonTrap
	MagicTrap -> magicTrap
	_ -> error "unknown trap"
-- | converts cell to a trap item if it's possible
trapFromCell :: Cell -> Object
trapFromCell = trapFromTerrain . terrain
-- | standard food ration for a soldier
foodRation :: Object
foodRation = Food {
	title = "food ration",
	nutrition = 200,
	weight' = 20,
	rotRate = 0,
	rotTime = 1,
	effect = id,
	isBerry = False,
	idO = -1
}

pickAxe :: Object
-- | list of all items
tOOLS :: [Object]
tOOLS = [pickAxe]
-- | pick axe to extract stone
pickAxe = Tool {
	title = "pick axe",
	tooltype = PickAxe,
	weight' = 50,
	charge = 20,
	idO = 0
}

strawberry, wolfberry, belladonna :: Object
-- | list of all berries
bERRIES :: [Object]
bERRIES = [strawberry, wolfberry, belladonna]
-- | get a berry by title, nutrition, id and effect
getBerry :: String -> Int -> Int -> (Monster -> Monster) -> Object
getBerry title' nutr id' eff = Food {
	title = title',
	nutrition = nutr,
	weight' = 10,
	rotRate = 1,
	rotTime = 100,
	effect = eff,
	isBerry = True,
	idO = id'
}

-- | just a strawberry without any effects
strawberry = getBerry "strawberry" 30 0 id
-- | just a berry with negative nutrition
wolfberry = getBerry "wolfberry" (-30) 1 id
-- | poisons you
belladonna = getBerry "belladonna" 0 2 $ setMaxTemp Poison $ Just 5
