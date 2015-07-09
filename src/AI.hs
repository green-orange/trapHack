module AI where

import Data
import Move
import Object
import ObjectOverall
import Changes
import Utils4AI
import Parts
import Monsters
import Utils4mon
import Colors
import Texts
import Ivy
import Golem
import GarbageCollector
import AIrepr
import DataWorld
import DataMonster
import DataDef

import System.Random (randomR, StdGen)
import Data.Maybe (fromJust, isNothing)
import UI.HSCurses.Curses (Key (..))
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

runAIattackIfClose :: Maybe (Elem, Int) -> AIfunc -> AIfunc
runAIattackIfClose Nothing = id
runAIattackIfClose (Just (e, d)) = attackIfClose e d

runAIpure :: AIpure -> AIfunc
runAIpure aip = case aip of
	NothingAI -> \_ _ w -> w
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
acceleratorAI f x y w = f x y $ changeMon newMon w where
	oldMon = getFirst w 
	newMon = oldMon {slowness = max 1 $ slowness oldMon - 7}
	
trollAI :: AIfunc -> AIfunc
trollAI f x y w = 
	if any (<= 5) $ map hp $ filter (\p -> kind p == hEAD || kind p == bODY) 
		$ parts $ getFirst w
	then addMessage (msgTrollDeath, bLUE) 
		$ changeMon (rock $ stdgen w) w
	else f x y w

rock :: StdGen -> Monster
rock g = fst $ getMonster (getPureAI NothingAI) [(getMain 0, (100, 5000))] 
	20 ((0,0),0.0) emptyInv 10000 g

mODSAI :: [AImod]
mODSAI = [HealAI, ZapAttackAI, PickAI, FireAI, WieldLauncherAI, WieldWeaponAI, 
	BindArmorAI, UseItemsAI]
		
healAI :: AIfunc -> AIfunc
healAI f x y w = 
	if canBeHealed (getFirst w) && needToBeHealedM (getFirst w)
	then fst $ quaffFirst (KeyChar $ healingAI w) w
	else f x y w
	
zapAttackAI :: AIfunc -> AIfunc
zapAttackAI f xPlayer yPlayer w = 
	if canZapToAttack (getFirst w) && isOnLine 5 xNow yNow xPlayer yPlayer
	then zapMon (undir dx dy) (zapAI w) w
	else f xPlayer yPlayer w where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
		
pickAI :: AIfunc -> AIfunc
pickAI f x y w =
	if not $ null objects
	then fromJust $ fst $ pickFirst $ foldr (changeChar . KeyChar) w alphabet
	else f x y w where
		xNow = xFirst w
		yNow = yFirst w
		objects = filter (\(x', y', _, _) -> x' == xNow && y' == yNow) $ items w

fireAI :: AIfunc -> AIfunc
fireAI f xPlayer yPlayer w =
	if canFire (getFirst w) && isOnLine (max maxX maxY) xNow yNow xPlayer yPlayer
	then fireMon (undir dx dy) (missileAI w) w
	else f xPlayer yPlayer w
	where
		(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w

bindSomethingAI :: Slot -> Int -> (World -> Maybe Char) -> AIfunc -> AIfunc
bindSomethingAI sl knd getter f x y w = 
	if null emptyParts
	then f x y w
	else case getter w of
		Nothing -> f x y w
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
useItemsAI f x y w = case useSomeItem objs keys of
	Nothing -> f x y w
	Just g -> g w
	where
		invList = M.toList $ inv $ getFirst w
		objs = map (fst . snd) invList
		keys = map (KeyChar . fst) invList

attackIfClose :: Elem -> Int -> AIfunc -> AIfunc
attackIfClose elem' dist f x y w =
	if abs dx <= dist && abs dy <= dist && 
		(abs dx > 1 || abs dy > 1 || not (any isLowerLimb $ parts $ getFirst w))
	then attackElem elem' dx dy w
	else f x y w
	where
		xNow = xFirst w
		yNow = yFirst w
		dx = x - xNow
		dy = y - yNow
	
stupidAI, stupidParalysisAI, stupidPoisonAI, stupidConfAI :: AIfunc
stupidAI = stupidFooAI moveFirst
stupidParalysisAI = stupidFooAI (\x y -> moveFirst x y . paralyse x y)
stupidPoisonAI = stupidFooAI (\x y -> moveFirst x y . 
	addTempByCoords Poison (5, 15) x y)
stupidConfAI = stupidFooAI (\x y -> moveFirst x y . 
	addTempByCoords Conf (0, 6) x y)

stupidFooAI :: (Int -> Int -> World -> World) -> AIfunc
stupidFooAI foo xPlayer yPlayer w = newWorld where
	g = stdgen w
	(xNow, yNow, dx, dy) = coordsFromWorld xPlayer yPlayer w
	(dx1, dy1, dx2, dy2)
		| dx == 0 = (1, dy, -1, dy)
		| dy == 0 = (dx, 1, dx, -1)
		| otherwise = (dx, 0, 0, dy)
	(dx', dy', newStdGen)
		| isValid w xNow yNow dx dy || abs (xPlayer - xNow) <= 1 
			&& abs (yPlayer - yNow) <= 1 = (dx, dy, g)
		| isValid w xNow yNow dx1 dy1 = (dx1, dy1, g)
		| isValid w xNow yNow dx2 dy2 = (dx2, dy2, g)
		| otherwise = let
			(rx, g') = randomR (-1, 1) g
			(ry, g'') = randomR (-1, 1) g'
			in (rx, ry, g'')
	newWorld = foo dx' dy' $ changeGen newStdGen w

stupidestAI :: AIfunc
stupidestAI xPlayer yPlayer world = 
	newWorld
	where
		xNow = xFirst world
		yNow = yFirst world
		dx = signum $ xPlayer - xNow
		dy = signum $ yPlayer - yNow
		newWorld = moveFirst dx dy world
				
randomAI :: AIfunc
randomAI _ _ w  = moveFirst rx ry newWorld where
	g = stdgen w
	(rx, g') = randomR (-1, 1) g
	(ry, g'') = randomR (-1, 1) g'
	newWorld = changeGen g'' w
	
wormAI :: AIfunc
wormAI xPlayer yPlayer w = 
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
tailWorm = getMonster  (getPureAI NothingAI) [(getMain 0, (100, 200))] 
	16 ((0,0),0.0) emptyInv 10000

