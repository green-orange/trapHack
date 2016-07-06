module Monsters.AI where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Data.ID
import Utils.Changes
import Utils.AI
import Utils.Monsters
import Items.Items
import Items.ItemsOverall
import Monsters.Move
import Monsters.Parts
import Monsters.Monsters
import Monsters.AIrepr
import Monsters.PartsList
import IO.Colors
import IO.Texts

import System.Random (randomR, StdGen)
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Function (on)
import Data.Functor ((<$>))

-- | run enumerable AImod type
runAImod :: AImod -> AIfunc -> AIfunc
runAImod aimod = case aimod of
	AcceleratorAI -> acceleratorAI
	TrollAI -> trollAI
	HealAI -> healAI
	ZapAttackAI -> zapAttackAI
	PickAI -> pickAI
	FireAI -> fireAI
	WieldLauncherAI -> wieldLauncherAI
	WieldWeaponAI -> wieldWeaponAI
	BindArmorAI -> bindArmorAI
	UseItemsAI -> useItemsAI
	EatAI -> eatAI

-- | run attackIfClose parameter
runAIattackIfClose :: Maybe (Elem, Int) -> AIfunc -> AIfunc
runAIattackIfClose Nothing = id
runAIattackIfClose (Just (e, d)) = attackIfClose e d

-- | run basic AI types
runAIpure :: AIpure -> AIfunc
runAIpure aip = case aip of
	NothingAI -> \_ _ _ w -> w
	StupidestAI -> stupidestAI
	StupidAI -> stupidAI
	StupidParalysisAI -> stupidParalysisAI
	StupidPoisonAI -> stupidPoisonAI
	StupidConfAI -> stupidConfAI
	RandomAI -> randomAI
	WormAI -> wormAI
	IvyAI -> breedAI getIvy
	BushAI -> breedAI getBush
	CollectorAI -> collectorAI
	GolemAI -> golemAI
	CleverSAI -> cleverSafeAI
	CleverVSAI -> cleverVerySafeAI
	CleverUAI -> cleverUnsafeAI

-- | run full AIrepr record
runAI :: AIrepr -> AIfunc
runAI repr =  foldr ((.) . runAImod) id (mods repr) $ 
	runAIattackIfClose (attackIfCloseMode repr) $ runAIpure $ aipure repr

-- | monster's speed increases every step
acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f x y p w = f x y p $ changeMon newMon w where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}

-- | monster can became a 'rock' instead of death
trollAI :: AIfunc -> AIfunc
trollAI f x y p w = 
	if any (<= 5) $ hp <$> filter (\pt -> kind pt == Head || kind pt == Body)
		(parts $ getFirst w)
	then addMessage (msgTrollDeath, blue) 
		$ changeMon (rock $ stdgen w) w
	else f x y p w

-- | technical "monster" for 'trollAI'
rock :: StdGen -> Monster
rock = fst . getMonster (getPureAI NothingAI) partsRck
	idRck ((0,0),0.0) emptyInv 10000 1 0

-- | list of all normal modificators (for Forgotten Beasts) 
modsAI :: [AImod]
modsAI = [HealAI ..]

-- | monster can use healing
healAI :: AIfunc -> AIfunc
healAI f x y p w = 
	if canBeHealed (getFirst w) && needToBeHealedM (getFirst w)
	then fst $ quaffFirst (healingAI w) w
	else f x y p w

-- | mosnter can use attack wands
zapAttackAI :: AIfunc -> AIfunc
zapAttackAI f xPlayer yPlayer p w = 
	if not p && canZapToAttack (getFirst w) 
		&& isOnLine 5 xNow yNow xPlayer yPlayer
	then zapMon (undir dx dy) (zapAI w) w
	else f xPlayer yPlayer p w where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w

-- | monster will pick all items
pickAI :: AIfunc -> AIfunc
pickAI f x y p w =
	if isItemHere w
	then case pickFirst $ foldr changeChar w alphabet of
		(Nothing, _) -> putWE "pickAI"
		(Just rez, _) -> rez
	else f x y p w

-- | monster can fire all missiles
fireAI :: AIfunc -> AIfunc
fireAI f xPlayer yPlayer p w =
	if not p && canFire (getFirst w) 
		&& isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI w) w
	else f xPlayer yPlayer p w
	where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w

-- | monster can bind items to given kind of part and slot;
-- 'getter' is a function to choose this item
bindSomethingAI :: Slot -> PartKind -> (World -> Maybe Char) -> AIfunc -> AIfunc
bindSomethingAI sl knd getter f x y p w = 
	case emptyParts of
		[] -> f x y p w
		(part, _):_ -> case getter w of
			Nothing -> f x y p w
			Just c -> bindMon sl c part w
	where
		mon = getFirst w
		emptyParts = filter ((\o -> kind o == knd) . snd) 
			$ filter (isEmptyPart sl mon . snd) $ zip [0..] $ parts mon

-- | specification of 'bindSomethingAI' to wield smth
wieldSomethingAI :: (World -> Maybe Char) -> AIfunc -> AIfunc
wieldSomethingAI = bindSomethingAI WeaponSlot Arm

wieldLauncherAI, wieldWeaponAI :: AIfunc -> AIfunc
-- | monster can wield launcher
wieldLauncherAI = wieldSomethingAI launcherAI
-- | monster can wield weapon
wieldWeaponAI = wieldSomethingAI weaponAI

-- | monster can bind armor of the given kind
bindArmorByKind :: PartKind -> AIfunc -> AIfunc
bindArmorByKind knd = bindSomethingAI ArmorSlot knd $ getArmorByKind knd

-- | monster can bind armor of all existence kinds
bindArmorAI :: AIfunc -> AIfunc
bindArmorAI = foldr ((.) . bindArmorByKind) id [Body ..]

-- | monster can use some useful items
useItemsAI :: AIfunc -> AIfunc
useItemsAI f x y p w = case useSomeItem objs keys of
	Nothing -> f x y p w
	Just g -> g w
	where
		invList = M.toList $ inv $ getFirst w
		objs = (fst . snd) <$> invList
		keys = fst <$> invList

-- | monster can eat smth
eatAI :: AIfunc -> AIfunc
eatAI f x y p w = 
	if canEat (getFirst w) && needEat (getFirst w)
	then fst $ eatFirst (foodAI w) w
	else f x y p w

-- | monster can distance attack with given element and range
attackIfClose :: Elem -> Int -> AIfunc -> AIfunc
attackIfClose elem' dist f x y peace w =
	if abs dx <= dist && abs dy <= dist && not peace &&
		(abs dx > 1 || abs dy > 1 || not (any isLowerLimb $ parts $ getFirst w))
	then attackElem elem' dx dy w
	else f x y peace w
	where
		xNow = xFirst w
		yNow = yFirst w
		dx = x - xNow
		dy = y - yNow
	
stupidAI, stupidParalysisAI, stupidPoisonAI, stupidConfAI :: AIfunc
-- | standard stupid AI which can't walk around big obstacles
stupidAI = stupidFooAI (\x y _ -> fst . moveFirst x y)
-- | stansard stupid AI with paralysis attack
stupidParalysisAI = stupidFooAI (\x y _ -> fst . moveFirst x y . paralyse x y)
-- | standard stupid AI with poison attack
stupidPoisonAI = stupidFooAI (\x y _ -> fst . moveFirst x y . 
	addTempByCoords Poison (5, 15) x y)
-- | standard stupid AI with confusion attack
stupidConfAI = stupidFooAI (\x y _ -> fst . moveFirst x y . 
	addTempByCoords Conf (0, 6) x y)

-- | stupid AI with some action (type of attack)
stupidFooAI :: AIfunc -> AIfunc
stupidFooAI foo xPlayer yPlayer peace w = newWorld where
	g = stdgen w
	(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
	(dx1, dy1, dx2, dy2)
		| dx == 0 = (1, dy, -1, dy)
		| dy == 0 = (dx, 1, dx, -1)
		| otherwise = (dx, 0, 0, dy)
	(dx', dy', newStdGen)
		| isValidAndSafe w xNow yNow dx dy || abs (xPlayer - xNow) <= 1 
			&& abs (yPlayer - yNow) <= 1 && not peace = (dx, dy, g)
		| isValidAndSafe w xNow yNow dx1 dy1 = (dx1, dy1, g)
		| isValidAndSafe w xNow yNow dx2 dy2 = (dx2, dy2, g)
		| otherwise = let
			(rx, g') = randomR (-1, 1) g
			(ry, g'') = randomR (-1, 1) g'
			in (rx, ry, g'')
	newWorld = foo dx' dy' peace w {stdgen = newStdGen}

-- | very stupid AI: monster can move only directly to you
stupidestAI :: AIfunc
stupidestAI xPlayer yPlayer peace w = 
	newWorld
	where
		xNow = xFirst w
		yNow = yFirst w
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		(dx', dy') = 
			if isValidAndSafe w xNow yNow dx dy
				|| not (isValid w xNow yNow dx dy) && not peace
				|| isFlying (getFirst w)
			then (dx, dy)
			else (0, 0)
		newWorld = fst $ moveFirst dx' dy' w

-- | AI with random direction of moving	
randomAI :: AIfunc
randomAI _ _ _ w  = fst $ moveFirst rx ry newWorld where
	g = stdgen w
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = w {stdgen = g''}

-- | AI for worm; it grows to you and die with some probability if
-- it move to cell with tail
wormAI :: AIfunc
wormAI xPlayer yPlayer _ w = 
	(case maybeMon of
		Nothing ->
			if isSafe w xNow yNow dx dy
			then spawnMon tailWorm xNow yNow . fst . moveFirst dx dy
			else fst . moveFirst dx dy
		Just mon ->
			if name mon == "Tail" && p < 0.2
			then killFirst
			else fst . moveFirst dx dy) w {stdgen = g}
	where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
		xNew = xNow + dx
		yNew = yNow + dy
		maybeMon = M.lookup (xNew, yNew) (units w)
		p :: Float
		(p, g) = randomR (0.0, 1.0) $ stdgen w

-- | just tail of the worm
tailWorm :: MonsterGen
tailWorm = getMonster (getPureAI NothingAI) partsTai
	idTai ((0,0),0.0) emptyInv 10000 1 0

-- | AI for Garbage Collector: if moves to nearest item
-- on the ground and pick it
collectorAI :: AIfunc
collectorAI _ _ _ world = 
	if isItemHere world
	then case pickFirst $ foldr changeChar world alphabet of
		(Nothing, _) -> putWE "collectorAI"
		(Just rez, _) -> rez
	else stupidAI xItem yItem False world
	where
		(xItem, yItem, _, _) = 
			if null (items world)
			then (0, 0, putWE "collectorAI", putWE "collectorAI")
			else minimumBy cmp $ items world
		dist x y = max (x - xFirst world) (y - yFirst world)
		cmp = on compare (\(x, y, _, _) -> dist x y)

-- | AI for Golem: just stand on the one placeand attack all enemies
golemAI :: AIfunc
golemAI _ _ _ world = case nears of
	[] -> world
	x:_ -> fst $ uncurry moveFirst x world
	where
		xNow = xFirst world
		yNow = yFirst world
		needToAttack (dx, dy) = 
			case M.lookup (xNow + dx, yNow + dy) $ units world of
				Nothing -> False
				Just mon -> isEnemy mon
		d = [-1, 0, 1]
		nears = filter needToAttack [(dx, dy) | dx <- d, dy <- d]

-- | AI for breeders: grow in the random direction or attack you if can 
breedAI :: MonsterGen -> AIfunc
breedAI mgen xPlayer yPlayer peace world
	| abs dx <= 1 && abs dy <= 1 && not peace = fst $ moveFirst dx dy world
	| isEmpty world xNew yNew && not (isItem xNew yNew world)
		= spawnMon mgen xNew yNew world {stdgen = g''}
	| otherwise = killFirst world {stdgen = g''} where
		xNow = xFirst world
		yNow = yFirst world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
		xNew = xNow + dx'
		yNew = yNow + dy'

-- | Ivy monster; it MUST have low speed (at most like now) 
getIvy :: MonsterGen
getIvy = getMonster (getPureAI IvyAI) partsIvy idIvy
	((2,10), 0.0) emptyInv 400 100 0
-- | Bush monster is a bit faster than Ivy
getBush :: MonsterGen
getBush = getMonster (getPureAI BushAI) partsBsh
	idBsh ((1, 10), 0.0) emptyInv 300 100 0

cleverSafeAI, cleverVerySafeAI, cleverUnsafeAI :: AIfunc
-- | clever AI version when monster doesn't recieve big wounds
cleverSafeAI = cleverAI $ isSafe &&& isValidOrPlayer
-- | clever AI version when monster doesn't recieve any bounds
cleverVerySafeAI = cleverAI $ isVerySafe &&& isValidOrPlayer
-- | clever AI version ignoring any bounds
cleverUnsafeAI = cleverAI isValidOrPlayer

-- | monster with clever AI can walk around any obstacles
cleverAI :: (World -> Int -> Int -> Int -> Int -> Bool) -> AIfunc
cleverAI safetyFun xPlayer yPlayer _ w = case maybeDir of
	Nothing -> w
	Just (dx, dy) -> fst $ moveFirst dx dy w
	where maybeDir = runAStar safetyFun (xPlayer, yPlayer) (xFirst w, yFirst w) w

