module Utils4AI where

import Data
import Utils4objects
import Object
import Changes
import DataWorld
import DataMonster
import DataDef

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import UI.HSCurses.Curses (Key (..))
import System.Random (randomR)

needToBeHealedM :: Monster -> Bool
needToBeHealedM mon =
	any (\x -> kind x == bODY && needToBeHealed x) $ parts mon

needToBeHealed :: Part -> Bool
needToBeHealed part = 2 * hp part < maxhp part

canBeHealed :: Monster -> Bool
canBeHealed mon = M.foldl (||) False $ M.map (isHealing . fst) $ inv mon

isHealing :: Object -> Bool
isHealing obj = title obj == "potion of healing"

healingAI :: World -> Char
healingAI world = fst $ M.findMin $ M.filter (isHealing . fst) $ inv $ getFirst world

canZapToAttack :: Monster -> Bool
canZapToAttack mon = M.foldl (||) False $ M.map (isAttackWand . fst) $ inv mon

canFire :: Monster -> Bool
canFire mon = any (isValidMissile mon) alphabet
	
isValidMissile :: Monster -> Char -> Bool
isValidMissile mon c = 
	isJust objs && isMissile obj && not (null intended) where
		objs = M.lookup c $ inv mon
		obj = fst $ fromJust objs
		launchers = filter isLauncher $ map (fst . fromJust) $ filter isJust 
			$ map (flip M.lookup (inv mon) . (\p -> objectKeys p 
			!! fromEnum WeaponSlot)) $ parts mon
		intended = filter (\w -> launcher obj == category w) launchers

haveLauncher :: Monster -> Bool
haveLauncher mon = M.foldl (||) False $ M.map (isLauncher . fst) $ inv mon

isAttackWand :: Object -> Bool
isAttackWand obj = isWand obj && charge obj > 0 && 
	elem (title obj) ["wand of striking", "wand of radiation",
	"wand of poison", "wand of slowing", "wand of stun"]

zapAI :: World -> Char
zapAI world = fst $ M.findMin $ M.filter (isAttackWand . fst) $ inv $ getFirst world

missileAI :: World -> Char
missileAI world = head $ filter (isValidMissile mon) alphabet where
	mon = getFirst world

safeMinFst :: (Ord k) => M.Map k a -> Maybe k
safeMinFst m = 
	if M.null m
	then Nothing
	else Just $ fst $ M.findMin m

getterByCond :: (Object -> Bool) -> World -> Maybe Char
getterByCond cond world = safeMinFst $ M.filterWithKey fun $ inv $ getFirst world where
	fun c (o, _) = not (isExistingBindingFirst world c) && cond o

weaponAI, launcherAI :: World -> Maybe Char
weaponAI = getterByCond isWeapon
launcherAI = getterByCond isLauncher

isArmorByKind :: Int -> Object -> Bool
isArmorByKind knd obj = isArmor obj && bind obj == knd

getArmorByKind :: Int -> World -> Maybe Char
getArmorByKind = getterByCond . isArmorByKind

isOnLine :: Int -> Int -> Int -> Int -> Int -> Bool
isOnLine d x1 y1 x2 y2 = abs (x1 - x2) <= d && abs (y1 - y2) <= d &&
	(x1 == x2 || y1 == y2 || x1 - y1 == x2 - y1 || x1 + y1 == x2 + y2)

undir :: Int -> Int -> Key
undir   0  (-1) = KeyChar 'k'
undir   0    1  = KeyChar 'j'
undir (-1)   0  = KeyChar 'h'
undir   1    0  = KeyChar 'l'
undir (-1) (-1) = KeyChar 'y'
undir   1  (-1) = KeyChar 'u'
undir (-1)   1  = KeyChar 'b'
undir   1    1  = KeyChar 'n'
undir   0    0  = KeyChar '.'
undir   _    _  = error "wrong direction (in function undir)"

usefulItem :: Object -> Key -> Maybe (World -> World)
usefulItem obj c
	| title obj == "potion of intellect" ||
		title obj == "potion of mutation" =
		Just $ fst . quaffFirst c
	| title obj == "wand of speed" && charge obj > 0 =
		Just $ zapMon (KeyChar '.') (fromKey c)
	| otherwise = Nothing
	
useSomeItem :: [Object] -> [Key] -> Maybe (World -> World)
useSomeItem [] _ = Nothing
useSomeItem _ [] = Nothing
useSomeItem (obj:objs) (c:cs) = case usefulItem obj c of
	Nothing -> useSomeItem objs cs
	f -> f

addTempByCoords :: Temp -> (Int, Int) -> Int -> Int -> World -> World
addTempByCoords t durs dx dy w = changeGen g $ changeMons newMons w where
	(dur, g) = randomR durs $ stdgen w
	xNew = xFirst w + dx
	yNew = yFirst w + dy
	maybeMon = M.lookup (xNew, yNew) $ units w
	newMons = case maybeMon of
		Nothing -> units' w
		Just mon -> insertU (xNew, yNew) (setMaxTemp t (Just dur) mon) 
			$ units' w

coordsFromWorld :: Int -> Int -> World -> (Int, Int, Int, Int)
coordsFromWorld xP yP w = 
	(xNow, yNow, signum $ xP - xNow, signum $ yP - yNow) where
		xNow = xFirst w
		yNow = yFirst w
