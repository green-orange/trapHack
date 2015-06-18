module Utils4AI where

import Data
import Parts
import Utils4objects
import Object
import Changes

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import UI.HSCurses.Curses (Key (..))

needToBeHealedM :: Monster -> Bool
needToBeHealedM mon =
	foldl (||) False $ map (\x -> kind x == bODY && needToBeHealed x) $ parts mon

needToBeHealed :: Part -> Bool
needToBeHealed part = 2 * (hp part) < maxhp part

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
	isJust objs && weapon mon /= ' '
	&& isLauncher weap && isMissile obj
	&& launcher obj == category weap where
	objs = M.lookup c $ inv mon
	obj = fst $ fromJust $ objs
	weap = fst $ (M.!) (inv mon) (weapon mon)

haveLauncher :: Monster -> Bool
haveLauncher mon = M.foldl (||) False $ M.map (isLauncher . fst) $ inv mon

isAttackWand :: Object -> Bool
isAttackWand obj = isWand obj && charge obj > 0 && 
	title obj == "wand of striking" ||
	title obj == "wand of radiation"

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

launcherAI :: World -> Maybe Char
launcherAI world = safeMinFst $ M.filter (isLauncher . fst) $ inv $ getFirst world

weaponAI :: World -> Maybe Char
weaponAI world = safeMinFst $ M.filter (isWeapon . fst) $ inv $ getFirst world

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
usefulItem obj c = 
	if
		title obj == "potion of intellect" ||
		title obj == "potion of mutation"
	then Just $ fst . quaffFirst c
	else if
		title obj == "wand of speed" && charge obj > 0
	then Just $ zapMon (KeyChar '.') (fromKey c)
	else Nothing
	
useSomeItem :: [Object] -> [Key] -> Maybe (World -> World)
useSomeItem [] _ = Nothing
useSomeItem _ [] = Nothing
useSomeItem (obj:objs) (c:cs) = case usefulItem obj c of
	Nothing -> useSomeItem objs cs
	f -> f


