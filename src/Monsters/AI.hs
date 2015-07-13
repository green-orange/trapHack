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
import Monsters.Ivy
import Monsters.Golem
import Monsters.GarbageCollector
import Monsters.AIrepr
import IO.Colors
import IO.Texts

import System.Random (randomR, StdGen)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as M

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

runAI :: AIrepr -> AIfunc
runAI repr =  foldr ((.) . runAImod) id (mods repr) $ 
	runAIattackIfClose (attackIfCloseMode repr) $ runAIpure $ aipure repr

acceleratorAI :: AIfunc -> AIfunc
acceleratorAI f x y p w = f x y p $ changeMon newMon w where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f x y p w = 
	if any (<= 5) $ map hp $ filter (\pt -> kind pt == hEAD || kind pt == bODY) 
		$ parts $ getFirst w
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
	if not $ null objects
	then fromJust $ fst $ pickFirst $ foldr changeChar w alphabet
	else f x y p w where
		xNow = xFirst w
		yNow = yFirst w
		objects = filter (\(x', y', _, _) -> x' == xNow && y' == yNow) $ items w

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
	if null emptyParts
	then f x y p w
	else case getter w of
		Nothing -> f x y p w
		Just c -> bindMon sl c (fst $ head emptyParts) w
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
		objs = map (fst . snd) invList
		keys = map fst invList

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
stupidAI = stupidFooAI (\x y _ -> moveFirst x y)
stupidParalysisAI = stupidFooAI (\x y _ -> moveFirst x y . paralyse x y)
stupidPoisonAI = stupidFooAI (\x y _ -> moveFirst x y . 
	addTempByCoords Poison (5, 15) x y)
stupidConfAI = stupidFooAI (\x y _ -> moveFirst x y . 
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
		| isValid w xNow yNow dx dy || abs (xPlayer - xNow) <= 1 
			&& abs (yPlayer - yNow) <= 1 && not peace = (dx, dy, g)
		| isValid w xNow yNow dx1 dy1 = (dx1, dy1, g)
		| isValid w xNow yNow dx2 dy2 = (dx2, dy2, g)
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
			if not peace || isValid w xNow yNow dx dy
			then (dx, dy)
			else (0, 0)
		newWorld = moveFirst dx' dy' w
				
randomAI :: AIfunc
randomAI _ _ _ w  = moveFirst rx ry newWorld where
	g = stdgen w
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' w
	
wormAI :: AIfunc
wormAI xPlayer yPlayer _ w = 
	(if isNothing maybeMon
	then spawnMon tailWorm xNow yNow . moveFirst dx dy
	else if name mon == "Tail" && p < 0.2
	then killFirst
	else moveFirst dx dy) $ changeGen g w where
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

