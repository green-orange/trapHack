module Monsters.AI where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.AI
import Utils.Monsters
import Items.Items
import Items.ItemsOverall
import Monsters.Move
import Monsters.Parts
import Monsters.Monsters
import Monsters.AIrepr
import IO.Colors
import IO.Texts

import System.Random (randomR, StdGen)
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Function (on)
import Data.Functor ((<$>))

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

runAIattackIfClose :: Maybe (Elem, Int) -> AIfunc -> AIfunc
runAIattackIfClose Nothing = id
runAIattackIfClose (Just (e, d)) = attackIfClose e d

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
	IvyAI -> ivyAI
	CollectorAI -> collectorAI
	GolemAI -> golemAI
	CleverSAI -> cleverSafeAI
	CleverVSAI -> cleverVerySafeAI
	CleverUAI -> cleverUnsafeAI

runAI :: AIrepr -> AIfunc
runAI repr =  foldr ((.) . runAImod) id (mods repr) $ 
	runAIattackIfClose (attackIfCloseMode repr) $ runAIpure $ aipure repr

acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f x y p w = f x y p $ changeMon newMon w where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f x y p w = 
	if any (<= 5) $ hp <$> filter (\pt -> kind pt == hEAD || kind pt == bODY)
		(parts $ getFirst w)
	then addMessage (msgTrollDeath, bLUE) 
		$ changeMon (rock $ stdgen w) w
	else f x y p w

rock :: StdGen -> Monster
rock g = fst $ getMonster (getPureAI NothingAI) [(getMain 0, (100, 5000))] 
	20 ((0,0),0.0) emptyInv 10000 1 g

mODSAI :: [AImod]
mODSAI = [HealAI ..]
		
healAI :: AIfunc -> AIfunc
healAI f x y p w = 
	if canBeHealed (getFirst w) && needToBeHealedM (getFirst w)
	then fst $ quaffFirst (healingAI w) w
	else f x y p w
	
zapAttackAI :: AIfunc -> AIfunc
zapAttackAI f xPlayer yPlayer p w = 
	if not p && canZapToAttack (getFirst w) 
		&& isOnLine 5 xNow yNow xPlayer yPlayer
	then zapMon (undir dx dy) (zapAI w) w
	else f xPlayer yPlayer p w where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
		
pickAI :: AIfunc -> AIfunc
pickAI f x y p w =
	if isItemHere w
	then fromJust $ fst $ pickFirst $ foldr changeChar w alphabet
	else f x y p w

fireAI :: AIfunc -> AIfunc
fireAI f xPlayer yPlayer p w =
	if not p && canFire (getFirst w) 
		&& isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI w) w
	else f xPlayer yPlayer p w
	where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w

bindSomethingAI :: Slot -> Int -> (World -> Maybe Char) -> AIfunc -> AIfunc
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

wieldSomethingAI :: (World -> Maybe Char) -> AIfunc -> AIfunc
wieldSomethingAI = bindSomethingAI WeaponSlot aRM

wieldLauncherAI, wieldWeaponAI :: AIfunc -> AIfunc
wieldLauncherAI = wieldSomethingAI launcherAI
wieldWeaponAI = wieldSomethingAI weaponAI

bindArmorByKind :: Int -> AIfunc -> AIfunc
bindArmorByKind knd = bindSomethingAI ArmorSlot knd $ getArmorByKind knd

bindArmorAI :: AIfunc -> AIfunc
bindArmorAI = foldr ((.) . bindArmorByKind) id [bODY, hEAD, aRM, lEG]

useItemsAI :: AIfunc -> AIfunc
useItemsAI f x y p w = case useSomeItem objs keys of
	Nothing -> f x y p w
	Just g -> g w
	where
		invList = M.toList $ inv $ getFirst w
		objs = (fst . snd) <$> invList
		keys = fst <$> invList

eatAI :: AIfunc -> AIfunc
eatAI f x y p w = 
	if canEat (getFirst w) && needEat (getFirst w)
	then fst $ eatFirst (foodAI w) w
	else f x y p w

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
stupidAI = stupidFooAI (\x y _ -> fst . moveFirst x y)
stupidParalysisAI = stupidFooAI (\x y _ -> fst . moveFirst x y . paralyse x y)
stupidPoisonAI = stupidFooAI (\x y _ -> fst . moveFirst x y . 
	addTempByCoords Poison (5, 15) x y)
stupidConfAI = stupidFooAI (\x y _ -> fst . moveFirst x y . 
	addTempByCoords Conf (0, 6) x y)

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
	newWorld = foo dx' dy' peace $ changeGen newStdGen w

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
			then (dx, dy)
			else (0, 0)
		newWorld = fst $ moveFirst dx' dy' w
				
randomAI :: AIfunc
randomAI _ _ _ w  = fst $ moveFirst rx ry newWorld where
	g = stdgen w
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' w
	
wormAI :: AIfunc
wormAI xPlayer yPlayer _ w = 
	(if isNothing maybeMon && isSafe w xNow yNow dx dy
	then spawnMon tailWorm xNow yNow . fst . moveFirst dx dy
	else if isJust maybeMon && name mon == "Tail" && p < 0.2
	then killFirst
	else fst . moveFirst dx dy) $ changeGen g w where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
		xNew = xNow + dx
		yNew = yNow + dy
		maybeMon = M.lookup (xNew, yNew) (units w)
		Just mon = maybeMon
		p :: Float
		(p, g) = randomR (0.0, 1.0) $ stdgen w

tailWorm :: MonsterGen
tailWorm = getMonster (getPureAI NothingAI) [(getMain 0, (100, 200))] 
	16 ((0,0),0.0) emptyInv 10000 1

collectorAI :: AIfunc
collectorAI _ _ _ world = 
	if isItemHere world
	then fromJust $ fst $ pickFirst $ foldr changeChar world alphabet
	else stupidAI xItem yItem False world
	where
		(xItem, yItem, _, _) = 
			if null (items world)
			then (0, 0, lol, lol)
			else minimumBy cmp $ items world
		dist x y = max (x - xFirst world) (y - yFirst world)
		cmp = on compare (\(x, y, _, _) -> dist x y)

golemAI :: AIfunc
golemAI _ _ _ world = case nears of
	[] -> world
	x:_ -> fst $ uncurry moveFirst x world
	where
		xNow = xFirst world
		yNow = yFirst world
		needToAttack (dx, dy) = isJust mons && isEnemy mon where
			mons = M.lookup (xNow + dx, yNow + dy) $ units world
			Just mon = mons
		d = [-1, 0, 1]
		nears = filter needToAttack [(dx, dy) | dx <- d, dy <- d]

ivyAI :: AIfunc
ivyAI xPlayer yPlayer peace world
	| abs dx <= 1 && abs dy <= 1 && not peace = fst $ moveFirst dx dy world
	| isEmpty world xNew yNew && not (isItem xNew yNew world)
		= spawnMon getIvy xNew yNew $ changeGen g'' world
	| otherwise = changeGen g'' $ killFirst world where
		xNow = xFirst world
		yNow = yFirst world
		dx = xPlayer - xNow
		dy = yPlayer - yNow
		g = stdgen world
		(dx', g')  = randomR (-1, 1) g
		(dy', g'') = randomR (-1, 1) g'
		xNew = xNow + dx'
		yNew = yNow + dy'

getIvy :: MonsterGen
getIvy = getMonster (getPureAI IvyAI) [(getMain 2, (5, 15))] 15
	((2,10), 0.0) emptyInv 400 100

cleverSafeAI, cleverVerySafeAI, cleverUnsafeAI :: AIfunc
cleverSafeAI = cleverAI $ isSafe &&& isValidOrPlayer
cleverVerySafeAI = cleverAI $ isVerySafe &&& isValidOrPlayer
cleverUnsafeAI = cleverAI isValidOrPlayer

cleverAI :: (World -> Int -> Int -> Int -> Int -> Bool) -> AIfunc
cleverAI safetyFun xPlayer yPlayer _ w = case maybeDir of
	Nothing -> w
	Just (dx, dy) -> fst $ moveFirst dx dy w
	where maybeDir = runAStar safetyFun (xPlayer, yPlayer) (xFirst w, yFirst w) w

