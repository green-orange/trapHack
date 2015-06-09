module Object where

import Data
import Changes
import Utils4all
import Stuff
import Utils4stuff
import Utils4mon

import UI.HSCurses.Curses (Key(..))
import Data.Set (member, empty, size)
import Data.Maybe (fromJust)
import System.Random (StdGen)
import Control.Monad ((>=>))

dropFirst :: Key -> World -> Bool -> (World, Bool)
dropFirst c world ignoreMessages = rez where
	objects = filter (\(x, _, _) -> KeyChar x == c) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if c == KeyChar (weapon $ getFirst world) && (alive $ getFirst world)
		then (maybeAddMessage "You can't drop weapon that you wield!" 
			$ changeAction ' ' world, False)
		else (changeMon mon $ addNeutralMessage newMsg $ addItem (x, y, obj, cnt) 
			$ changeAction ' ' world, True)
	(_, obj, cnt) = head objects
	(x, y, oldmon) = head $ units world
	mon = delObj c $ oldmon
	newMsg =
		if ignoreMessages
		then ""
		else (name $ getFirst world) ++ " drop" ++ ending world ++ titleShow obj ++ "."

dropAll :: World -> World
dropAll world = foldr (\x y -> fst $ dropFirst x y True) world $ 
	map (KeyChar . first) $ inv $ getFirst world

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
	objects = filter (\(x, _, _) -> x == last (store world)) $ inv $ getFirst world
	rez =
		if (length objects == 0)
		then (maybeAddMessage "You haven't this item!" failWorld, False)
		else if (not $ isWand obj)
		then (maybeAddMessage "You don't know how to zap it!" failWorld, False)
		else if dir c == Nothing
		then (maybeAddMessage "It's not a direction!" failWorld, False)
		else if charge obj == 0
		then (maybeAddMessage "This wand has no charge!" failWorld, True)
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
zapMon dir obj world = fst $ zapFirst dir $
	changeStore (store world ++ [obj]) world

pickFirst :: World -> (Maybe World, String)
pickFirst world =
	if 0 == (size $ toPick world)
	then (Nothing, "")
	else let
		(xMon, yMon, oldMon) = head $ units world
		itemsWithIndices :: [((Int, Int, Object, Int), Int)]
		itemsWithIndices = addIndices (\(x', y' , _, _) -> xMon == x' && yMon == y') $ items world
		(itemsToPick, rest) = split (\(_, n) -> (n >= 0) && (n < length alphabet) 
			&& (member (alphabet !! n) $ toPick world)) itemsWithIndices
		newItems = map fst rest
		maybeInv = addInvs (inv oldMon) $ map (\(_,_,a,b) -> (a,b)) $ map fst itemsToPick
	in case maybeInv of
	Nothing -> (Nothing, "You knapsack is full!")
	Just newInv ->
		let
		mon = changeInv newInv oldMon
		color = 
			if isPlayerNow world
			then gREEN
			else yELLOW
		newMessage = message world ++ [(name mon ++ " pick" ++ ending world ++ "some objects.", color)]
		in (Just world {
			units = (xMon, yMon, mon) : (tail $ units world),
			message = newMessage,
			items = newItems,
			action = ' ',
			toPick = empty
		}, "")
		

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
	objects = filter (\(x, _, _) -> x == last (store world)) $ inv $ getFirst world
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
		else (changeStore (init $ store world) $ changeAction ' ' newWorld, True)
	(x, y, oldMon) = head $ units world
	maybeCoords = dirs world (x, y, dx, dy)
	cnt = min n $ count wielded
	newWorld = case maybeCoords of
		Just (xNew, yNew) -> foldr (.) id (replicate cnt $ 
			fire xNew yNew dx dy obj) $ changeMon (fulldel oldMon) world
		Nothing -> failWorld
	Just (dx, dy) = dir c
	[(_, obj, n)] = objects
	fulldel = foldr (.) id $ replicate cnt $ delObj $ KeyChar $ last $ store world
	failWorld = changeStore (init $ store world) $ changeAction ' ' world
	
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
fireMon dir obj world = fst $ fireFirst dir $
	changeStore (store world ++ [obj]) world

addInvs :: [Inv] -> [(Object, Int)] -> Maybe [Inv]
addInvs startInv items = (foldl (>=>) return $ map addInv items) startInv

addInv :: (Object, Int) -> [Inv] -> Maybe [Inv]
addInv (obj, cnt) list  =
	if null this
	then addInvWithAlphabet alphabet list (obj,cnt)
	else Just $ map change list
	where
		addInvWithAlphabet :: [Char] -> [Inv] -> (Object, Int) -> Maybe [Inv]
		addInvWithAlphabet [] _ _ = Nothing
		addInvWithAlphabet alph inv (obj, cnt) = 
			if length this == 0
			then Just $ (head alph, obj, cnt) : inv
			else addInvWithAlphabet (tail alph) inv (obj, cnt) where
				this = filter (\(x, _, _) -> x == head alph) inv
		this = filter (\(_,obj',_) -> obj' == obj) list
		change (c, o, n) = 
			if o == obj
			then (c, o, n + cnt)
			else (c, o, n)


	
