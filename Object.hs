module Object where

import Data
import Changes
import Utils4all
import Stuff
import Utils4stuff
import HealDamage

import UI.HSCurses.Curses (Key(..))
import Data.Maybe (fromJust)
import System.Random (StdGen)

quaffFirst :: Key -> World -> (World, Bool)
quaffFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if (not $ isPotion obj)
		then (maybeAddMessage "You don't know how to quaff it!"
			$ changeAction ' ' world, False)
		else (changeGen g $ changeMon mon' $ addNeutralMessage newMsg $ changeAction ' ' world, True)
	newMsg = (name $ getFirst world) ++ " quaff" ++ ending world ++ titleShow obj ++ "."
	[(_, obj, _)] = objects
	(x, y, oldMon) = head $ units world
	(mon, g) = act obj (oldMon, stdgen world)
	mon' = delObj c mon
	
readFirst :: Key -> World -> (World, Bool)
readFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if (not $ isScroll obj)
		then (maybeAddMessage "You don't know how to read it!"
			$ changeAction ' ' world, False)
		else (changeMon mon' $ addNeutralMessage newMsg $ changeAction ' ' newWorld, True)
	newMsg = (name $ getFirst world) ++ " read" ++ ending world ++ titleShow obj ++ "."
	[(_, obj, _)] = objects
	newWorld = actw obj world
	(x, y, mon) = head $ units newWorld
	mon' = delObj c mon

zapFirst :: Key -> World -> (World, Bool)
zapFirst c world = rez where
	objects = filter (\(x, _, _) -> x == prevAction world) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if (not $ isWand obj)
		then (maybeAddMessage "You don't know how to zap it!" failWorld, False)
		else if dir c == Nothing
		then (maybeAddMessage "It's not a direction!" failWorld, False)
		else if charge obj == 0
		then (maybeAddMessage "This wand has no charge!" failWorld, True)
		else (changeMon mon $ changeAction ' ' $ newMWorld, True)
	(x, y, _) = head $ units world
	(dx, dy) = fromJust $ dir c
	maybeCoords = dirs world (x, y, dx, dy)
	newMWorld = case maybeCoords of
		Just (xNew, yNew) -> zap world xNew yNew dx dy obj
		Nothing -> failWorld
	(_, _, oldMon) = head $ units newMWorld
	[(_, obj, _)] = objects
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
		decRange obj = obj {range = range obj - 1}
		actAll :: StdGen -> [(Int, Int, Monster)] -> ([(Int, Int, Monster)], StdGen)
		actAll g [] = ([], g)
		actAll g (o@(x',y',m):os) = (oNew : osNew, g'') where
			(osNew, g'') = actAll g' os
			(oNew, g') = 
				if (x == x') && (y == y')
				then
					let (newMon, newG) = act obj (m, g)
					in ((x, y, newMon), newG) 
				else (o, g)
		msgFilter (x', y', mon) = 
			if (x == x') && (y == y')
			then name mon ++ " was zapped!"
			else ""
		msg = foldl (++) "" $ map msgFilter $ units world
		(newMons, newG) = actAll (stdgen world) $ units world
		newMWorld = changeGen newG $ addNeutralMessage msg $ changeMons newMons world

zapMon :: Key -> Char -> World -> World
zapMon dir obj world = fst $ zapFirst dir $ world {prevAction = obj}
		
trapFirst :: Key -> World -> (World, Bool)
trapFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if (not $ isTrap obj)
		then (maybeAddMessage "It's not a trap!" failWorld, False)
		else (addNeutralMessage newMsg $ changeMon mon $ changeMap x y (num obj) $ changeAction ' ' $ world, True)
	(x, y, oldMon) = head $ units world
	[(_, obj, _)] = objects
	mon = delObj c oldMon
	failWorld = changeAction ' ' world
	newMsg = (name oldMon) ++ " set" ++ ending world ++ title obj ++ "."
	
untrapFirst :: World -> (World, Bool)
untrapFirst world = rez where
	rez =
		if not $ isUntrappable $ worldmap world !! x !! y
		then (maybeAddMessage "It's nothing to untrap here!" failWorld, False)
		else (addItem (x, y, trap, 1) $ addNeutralMessage newMsg $ changeMap x y eMPTY 
			$ changeAction ' ' $ world, True)
	(x, y, mon) = head $ units world
	failWorld = changeAction ' ' world
	trap = trapFromTerrain $ worldmap world !! x !! y
	newMsg = (name mon) ++ " untrap" ++ ending world ++ title trap ++ "."
	
wieldFirst :: Key -> World -> (World, Bool)
wieldFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if not (isWeapon obj || isLauncher obj)
		then (maybeAddMessage "You don't know how to wield it!" failWorld, False)
		else (addNeutralMessage newMsg $ changeMon mon $ changeAction ' ' $ world, True)
	(x, y, oldMon) = head $ units world
	[(_, obj, _)] = objects
	mon = changeWeapon c oldMon
	failWorld = changeAction ' ' world
	newMsg = (name oldMon) ++ " wield" ++ ending world ++ title obj ++ "."
	
fireFirst :: Key -> World -> (World, Bool)
fireFirst c world = rez where
	objects = filter (\(x, _, _) -> x == prevAction world) $ inv $ getFirst world
	wielded =
		if null listWield
		then Something
		else second $ head listWield
	listWield = filter (\(x, _, _) -> x == weapon oldMon) $ inv $ getFirst world
	rez =
		if null objects
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
	(x, y, oldMon) = head $ units world
	maybeCoords = dirs world (x, y, dx, dy)
	cnt = min n $ count wielded
	newWorld = case maybeCoords of
		Just (xNew, yNew) -> foldr (.) id (replicate cnt $ 
			fire xNew yNew dx dy obj) $ changeMon (fulldel oldMon) world
		Nothing -> failWorld
	Just (dx, dy) = dir c
	[(_, obj, n)] = objects
	fulldel = foldr (.) id $ replicate cnt $ delObj $ KeyChar $ prevAction world
	failWorld = changeAction ' ' world
	
fire :: Int -> Int -> Int -> Int -> Object -> World -> World
fire x y dx dy obj world = 
	if incorrect
	then world
	else if null mons
	then fire xNew yNew dx dy obj world
	else newWorld
	where
		mons = filter (\(x', y', _) -> x == x' && y == y') $ units world
		(incorrect, (xNew, yNew)) = case dirs world (x, y, dx, dy) of
			Nothing -> (True, (0, 0))
			Just p -> (False, p)
		(newDmg, g) = objdmg obj world
		(newMon, g') = dmgRandom newDmg (third $ head mons) g
		actFilter arg@(x', y', mon) = 
			if (x == x') && (y == y')
			then (x', y', newMon)
			else arg
		msg = case newDmg of
			Nothing -> capitalize (title obj) ++ " misses."
			Just _ -> capitalize (title obj) ++ " hits " ++ name (third $ head mons) ++ "."
		newWorld = addNeutralMessage msg $ changeGen g' $ changeMons (map actFilter $ units world) world
		
fireMon :: Key -> Char -> World -> World
fireMon dir obj world = fst $ fireFirst dir $ world {prevAction = obj}


	
