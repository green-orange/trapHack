module Object where

import Data
import Changes
import Stuff
import Utils4stuff
import HealDamage
import Messages
import Utils4objects
import Parts
import Utils4mon

import UI.HSCurses.Curses (Key(..))
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map as M

dir :: Key -> Maybe (Int, Int)
dir c = case c of
	KeyChar 'k' -> Just ( 0, -1)
	KeyChar 'j' -> Just ( 0,  1)
	KeyChar 'h' -> Just (-1,  0)
	KeyChar 'l' -> Just ( 1,  0)
	KeyChar 'y' -> Just (-1, -1)
	KeyChar 'u' -> Just ( 1, -1)
	KeyChar 'b' -> Just (-1,  1)
	KeyChar 'n' -> Just ( 1,  1)
	KeyChar '.' -> Just ( 0,  0)
	_           -> Nothing

quaffFirst :: Key -> World -> (World, Bool)
quaffFirst c world = rez where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM oldMon
		then (maybeAddMessage "You need arms to quaff a potion!" 
			$ changeAction ' ' world, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if not $ isPotion obj
		then (maybeAddMessage "You don't know how to quaff it!"
			$ changeAction ' ' world, False)
		else (changeGen g $ changeMon mon' $ addNeutralMessage newMsg $ changeAction ' ' world, True)
	newMsg = (name $ getFirst world) ++ " quaff" ++ ending world ++ titleShow obj ++ "."
	(obj, _) = fromJust objects
	oldMon = getFirst world
	(mon, g) = act obj (oldMon, stdgen world)
	mon' = delObj c mon
	
readFirst :: Key -> World -> (World, Bool)
readFirst c world = rez where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM mon
		then (maybeAddMessage "You need arms to read a scroll!" 
			$ changeAction ' ' world, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if not $ isScroll obj
		then (maybeAddMessage "You don't know how to read it!"
			$ changeAction ' ' world, False)
		else (changeMon mon' $ addNeutralMessage newMsg $ changeAction ' ' newWorld, True)
	newMsg = (name $ getFirst world) ++ " read" ++ ending world ++ titleShow obj ++ "."
	(obj, _) = fromJust objects
	newWorld = actw obj world
	mon = getFirst world
	mon' = delObj c mon

zapFirst :: Key -> World -> (World, Bool)
zapFirst c world = rez where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM oldMon
		then (maybeAddMessage "You need arms to zap a wand!" failWorld, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if not $ isWand obj
		then (maybeAddMessage "You don't know how to zap it!" failWorld, False)
		else if dir c == Nothing
		then (maybeAddMessage "It's not a direction!" failWorld, False)
		else if charge obj == 0
		then (maybeAddMessage "This wand has no charge!" failWorld, True)
		else (changeMon mon $ changeAction ' ' $ newMWorld, True)
	(dx, dy) = fromJust $ dir c
	maybeCoords = dirs world (x, y, dx, dy)
	newMWorld = case maybeCoords of
		Just (xNew, yNew) -> zap world xNew yNew dx dy obj
		Nothing -> failWorld
	x = xFirst world
	y = yFirst world
	oldMon = getFirst newMWorld
	(obj, _) = fromJust objects
	mon = decChargeByKey (prevAction newMWorld) $ oldMon
	failWorld = changeAction ' ' world

zap :: World -> Int -> Int -> Int -> Int -> Object -> World
zap world x y dx dy obj = 
	if (range obj == 0) || incorrect
	then world
	else if (dx == 0) && (dy == 0)
	then newMWorld
	else zap newMWorld xNew yNew dx dy $ decRange obj
	where
		(incorrect, (xNew, yNew)) = case dirs world (x, y, dx, dy) of
			Nothing -> (True, (0, 0))
			Just p -> (False, p)
		decRange :: Object -> Object
		decRange obj' = obj' {range = range obj - 1}
		msg = 
			case M.lookup (x, y) $ units world of
			Nothing -> ""
			Just mon ->
				if name mon == "You"
				then "You were zapped!"
				else name mon ++ " was zapped!"
		newMons = 
			case M.lookup (x, y) $ units world of
			Nothing -> units' world
			Just mon -> update x y $ (units' world) {list = 
				M.insert (x, y) (fst $ act obj (mon, stdgen world)) $ units world}
		newMWorld = addMessage (msg, color) $ changeMons newMons world
		color = 
			if isPlayerNow world
			then gREEN
			else case fmap isPlayer $ M.lookup (x, y) $ units world of
			Nothing    -> lol
			Just False -> bLUE
			Just True  -> rED

zapMon :: Key -> Char -> World -> World
zapMon dir' obj world = fst $ zapFirst dir' $ world {prevAction = obj}
		
trapFirst :: Key -> World -> (World, Bool)
trapFirst c world = rez where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM oldMon
		then (maybeAddMessage "You need arms to set a trap!" failWorld, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if not $ isTrap obj
		then (maybeAddMessage "It's not a trap!" failWorld, False)
		else (addNeutralMessage newMsg $ changeMon mon $ changeMap x y (num obj) $ changeAction ' ' $ world, True)
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	(obj, _) = fromJust objects
	mon = delObj c oldMon
	failWorld = changeAction ' ' world
	newMsg = (name oldMon) ++ " set" ++ ending world ++ title obj ++ "."
	
untrapFirst :: World -> (World, Bool)
untrapFirst world = rez where
	rez =
		if not $ hasPart aRM mon
		then (maybeAddMessage "You need arms to remove a trap!" failWorld, False)
		else if not $ isUntrappable $ worldmap world !! x !! y
		then (maybeAddMessage "It's nothing to untrap here!" failWorld, False)
		else (addItem (x, y, trap, 1) $ addNeutralMessage newMsg $ changeMap x y eMPTY 
			$ changeAction ' ' $ world, True)
	x = xFirst world
	y = yFirst world
	mon = getFirst world
	failWorld = changeAction ' ' world
	trap = trapFromTerrain $ worldmap world !! x !! y
	newMsg = (name mon) ++ " untrap" ++ ending world ++ title trap ++ "."
	
wieldFirst :: Key -> World -> (World, Bool)
wieldFirst c world = rez where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM oldMon
		then (maybeAddMessage "You need arms to wield a weapon!" failWorld, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if not (isWeapon obj || isLauncher obj)
		then (maybeAddMessage "You don't know how to wield it!" failWorld, False)
		else (addNeutralMessage newMsg $ changeMon mon $ changeAction ' ' $ world, True)
	oldMon = getFirst world
	(obj, _) = fromJust objects
	mon = changeWeapon c oldMon
	failWorld = changeAction ' ' world
	newMsg = (name oldMon) ++ " wield" ++ ending world ++ title obj ++ "."
	
fireFirst :: Key -> World -> (World, Bool)
fireFirst c world = rez where
	objects = M.lookup (prevAction world) $ inv $ getFirst world
	wielded =
		if isNothing listWield
		then Something
		else fst $ fromJust listWield
	listWield = M.lookup (weapon oldMon) $ inv $ getFirst world
	rez =
		if not $ hasPart aRM oldMon
		then (maybeAddMessage "You need arms to fire!" failWorld, False)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if not $ isMissile obj
		then (maybeAddMessage "You don't know how to fire it!" failWorld, False)
		else if weapon oldMon == ' '
		then (maybeAddMessage "You have no weapon!" failWorld, False)
		else if not $ isLauncher wielded
		then (maybeAddMessage "Your weapon is not intended for firing" failWorld, False)
		else if launcher obj /= category wielded
		then (maybeAddMessage ("You can't fire " ++ title obj ++ " by " 
			++ category wielded ++ "!") failWorld, False)
		else if dir c == Nothing
		then (maybeAddMessage "It's not a direction!" failWorld, False)
		else (changeAction ' ' newWorld, True)
	x = xFirst world
	y = yFirst world
	oldMon = getFirst world
	maybeCoords = dirs world (x, y, dx, dy)
	cnt = min n $ count wielded
	newWorld = case maybeCoords of
		Just (xNew, yNew) -> foldr (.) id (replicate cnt $ 
			fire xNew yNew dx dy obj) $ changeMon (fulldel oldMon) world
		Nothing -> failWorld
	Just (dx, dy) = dir c
	(obj, n) = fromJust objects
	fulldel = foldr (.) id $ replicate cnt $ delObj $ KeyChar $ prevAction world
	failWorld = changeAction ' ' world
	
fire :: Int -> Int -> Int -> Int -> Object -> World -> World
fire x y dx dy obj world = 
	if incorrect
	then world
	else if isNothing maybeMon
	then fire xNew yNew dx dy obj world
	else newWorld
	where
		maybeMon = M.lookup (x, y) $ units world 
		Just mon = maybeMon
		(incorrect, (xNew, yNew)) = case dirs world (x, y, dx, dy) of
			Nothing -> (True, (0, 0))
			Just p -> (False, p)
		(newDmg, g) = objdmg obj world
		(newMon, g') = dmgRandom newDmg mon g
		msg = case newDmg of
			Nothing -> capitalize (title obj) ++ " misses."
			Just _ -> capitalize (title obj) ++ " hits " ++ name mon ++ "."
		newWorld = addMessage (msg, color) $ changeGen g' 
			$ changeMons (insertU (x, y) newMon $ units' world) world
		color = 
			if isPlayerNow world
			then gREEN
			else case fmap isPlayer $ M.lookup (x, y) $ units world of
			Nothing    -> lol
			Just False -> bLUE
			Just True  -> rED
		
fireMon :: Key -> Char -> World -> World
fireMon dir' obj world = fst $ fireFirst dir' $ world {prevAction = obj}
