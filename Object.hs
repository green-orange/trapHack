module Object where

import Data
import Utils
import Changes
import Utils4all
import Stuff

import UI.HSCurses.Curses (Key(..))
import Data.Set (member, empty, size)
import Data.Maybe (fromJust)

dropFirst :: Key -> World -> Bool -> (World, Bool)
dropFirst c world ignoreMessages = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (addMessage (
				if isPlayerNow world
				then "You haven't this item!"
				else ""
			) $ changeAction ' ' world, False)
		else (changeMon mon $ addMessage newMsg $ addItem (x, y, obj, 1) 
			$ changeAction ' ' world, True)
	(_, obj, cnt) = head objects
	(x, y, oldmon) = head $ units world
	mon = delObj c $ oldmon
	newMsg =
		if ignoreMessages
		then message world
		else oldMessage world ++ (name $ getFirst world) ++ " drop" ++ ending world ++ titleShow obj ++ "."

dropAll :: World -> World
dropAll world = foldr (\x y -> fst $ dropFirst x y True) world (map KeyChar $ alphabet ++ notAlphabet)

quaffFirst :: Key -> World -> (World, Bool)
quaffFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (addMessage (
				if isPlayerNow world
				then "You haven't this item!"
				else ""
			) $ changeAction ' ' world, False)
		else if (not $ isPotion obj)
		then (addMessage (
				if isPlayerNow world
				then "You don't know how to quaff it!"
				else ""
			) $ changeAction ' ' world, False)
		else (changeMon mon $ addMessage newMsg $ changeAction ' ' world, True)
	newMsg = (name $ getFirst world) ++ " quaff" ++ ending world ++ titleShow obj ++ "."
	[(_, obj, _)] = objects
	(x, y, oldMon) = head $ units world
	mon = delObj c $ act obj $ oldMon

zapFirst :: Key -> World -> (World, Bool)
zapFirst c world = rez where
	objects = filter (\(x, _, _) -> x == last (store world)) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (addMessage (
				if isPlayerNow world
				then "You haven't this item!"
				else ""
			) failWorld, False)
		else if (not $ isWand obj)
		then (addMessage (
				if isPlayerNow world
				then "You don't know how to zap it!"
				else ""
			) failWorld, False)
		else if dir c == Nothing
		then (addMessage (
				if isPlayerNow world
				then "It's not a direction!"
				else ""
			) failWorld, False)
		else if charge obj == 0
		then (addMessage (
				if isPlayerNow world
				then "This wand has no charge!"
				else ""
			) failWorld, True)
		else (changeMon mon $ changeStore (init $ store world) $ changeAction ' ' $ newMWorld, True)
	(x, y, _) = head $ units world
	(dx, dy) = fromJust $ dir c
	maybeCoords = dirs world (x, y, dx, dy)
	newMWorld = case maybeCoords of
		Just (xNew, yNew) -> zap world xNew yNew dx dy obj
		Nothing -> failWorld
	(_, _, oldMon) = head $ units newMWorld
	[(_, obj, _)] = objects
	mon = decChargeByKey (last $ store newMWorld) $ oldMon
	failWorld = changeStore (init $ store world) $ changeAction ' ' world

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
		decRange obj = Wand {
			title = title obj,
			act = act obj,
			range = range obj - 1,
			charge = charge obj
		}
		actFilter arg@(x', y', mon) = 
			if (x == x') && (y == y')
			then (x, y, act obj $ mon)
			else arg
		msgFilter (x', y', mon) = 
			if (x == x') && (y == y')
			then name mon ++ " was zapped! "
			else ""
		msg = foldl (++) "" $ map msgFilter $ units world
		newMWorld = addMessage msg $ changeMons (map actFilter $ units world) world

zapMon :: Key -> Char -> World -> World
zapMon dir obj world = fst $ zapFirst dir $
	changeStore (store world ++ [obj]) world

pickFirst :: World -> Maybe World
pickFirst world =
	if 0 == (size $ toPick world)
	then Nothing
	else Just World {
		units = (xMon, yMon, mon) : (tail $ units world),
		message = newMessage,
		items = newItems,
		action = ' ',
		stdgen = stdgen world,
		wave = wave world,
		toPick = empty,
		store = store world,
		worldmap = worldmap world,
		dirs = dirs world
	} where
	(xMon, yMon, oldMon) = head $ units world
	itemsWithIndices :: [((Int, Int, Object, Int), Int)]
	itemsWithIndices = addIndices (\(x', y' , _, _) -> xMon == x' && yMon == y') $ items world
	(itemsToPick, rest) = split (\(_, n) -> (n >= 0) && (member (alphabet !! n) $ toPick world)) itemsWithIndices
	mon = Monster {
		ai = ai oldMon,
		parts = parts oldMon,
		x = x oldMon,
		y = y oldMon,
		name = name oldMon,
		stddmg = stddmg oldMon,
		inv = addInvs (inv oldMon) $ map (\(_,_,a,b) -> (a,b)) $ map fst itemsToPick,
		slowness = slowness oldMon,
		time = time oldMon
	}
	newItems = map fst rest
	newMessage = oldMessage world ++ name mon ++ " pick" ++ ending world ++ "some objects."

trapFirst :: Key -> World -> (World, Bool)
trapFirst c world = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (addMessage (
				if isPlayerNow world
				then "You haven't this item!"
				else ""
			) failWorld, False)
		else if (not $ isTrap obj)
		then (addMessage (
				if isPlayerNow world
				then "It's not a trap!"
				else ""
			) failWorld, False)
		else (addMessage newMsg $ changeMon mon $ changeMap x y (num obj) $ changeAction ' ' $ world, True)
	(x, y, oldMon) = head $ units world
	[(_, obj, _)] = objects
	mon = delObj c oldMon
	failWorld = changeAction ' ' world
	newMsg = (name oldMon) ++ " set" ++ ending world ++ title obj ++ "."
	
untrapFirst :: World -> (World, Bool)
untrapFirst world = rez where
	rez =
		if not $ isUntrappable $ worldmap world !! x !! y
		then (addMessage (
				if isPlayerNow world
				then "It's nothing to untrap here!"
				else ""
			) failWorld, False)
		else (addItem (x, y, trap, 1) $ addMessage newMsg $ changeMap x y eMPTY 
			$ changeAction ' ' $ world, True)
	(x, y, mon) = head $ units world
	failWorld = changeAction ' ' world
	trap = trapFromTerrain $ worldmap world !! x !! y
	newMsg = (name mon) ++ " untrap" ++ ending world ++ title trap ++ "."
	
	
