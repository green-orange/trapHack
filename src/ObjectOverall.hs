module ObjectOverall where

import Data
import Changes
import Utils4mon
import Messages
import Utils4objects
import Parts
import Colors

import Control.Monad ((>=>))
import qualified Data.Set as S
import UI.HSCurses.Curses (Key (..))
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)

dropFirst :: Key -> World -> Bool -> (World, Bool)
dropFirst c world ignoreMessages = rez where
	rez =
		if (isNothing objects)
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if isExistingBindingFirst world (fromKey c) 
			&& (alive $ getFirst world)
		then (maybeAddMessage "You can't drop the item wich you have equipped!" 
			$ changeAction ' ' world, False)
		else (changeMon mon $ addNeutralMessage newMsg $ addItem (x, y, obj, cnt) 
			$ changeAction ' ' world, True)
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	(obj, cnt) = fromJust objects
	x = xFirst world
	y = yFirst world
	oldmon = getFirst world
	mon = delAllObj c $ oldmon
	newMsg =
		if ignoreMessages
		then ""
		else (name $ getFirst world) ++ " drop" ++ ending world 
			++ titleShow obj ++ "."

dropAll :: World -> World
dropAll world = foldr (\x y -> fst $ dropFirst x y True) world $ 
	map (KeyChar . fst) $ M.toList $ inv $ getFirst world

pickFirst :: World -> (Maybe World, String)
pickFirst world =
	if 0 == (S.size $ chars world)
	then (Nothing, "")
	else let
		xMon = xFirst world
		yMon = yFirst world
		oldMon = getFirst world
		itemsWithIndices :: [((Int, Int, Object, Int), Int)]
		itemsWithIndices = addIndices (\(x', y' , _, _) -> 
			xMon == x' && yMon == y') $ items world
		(itemsToPick, rest) = split (\(_, n) -> (n >= 0) 
			&& (n < length alphabet) 
			&& (S.member (alphabet !! n) $ chars world)) itemsWithIndices
		newItems = map fst rest
		maybeInv = addInvs (inv oldMon) $ map (\(_,_,a,b) -> (a,b)) 
			$ map fst itemsToPick
	in case maybeInv of
	Nothing -> (Nothing, "You knapsack is full!")
	Just newInv ->
		let
		mon = changeInv newInv oldMon
		color = 
			if isPlayerNow world
			then gREEN
			else yELLOW
		newMessage = (name mon ++ " pick" ++ ending world 
			++ "some objects.", color) : message world
		in (Just world {
			units' = (units' world) 
				{getFirst' = mon, list = M.insert (xMon, yMon) mon $ units world},
			message = newMessage,
			items = newItems,
			action = ' ',
			chars = S.empty
		}, "")
		
dropManyFirst :: World -> Maybe World
dropManyFirst world =
	if 0 == (S.size $ chars world)
	then Nothing
	else Just newWorld where
		newMsg = name (getFirst world) ++ " drop" ++ ending world 
			++ "some objects."
		newWorld = changeAction ' ' $ changeChars S.empty 
			$ addNeutralMessage newMsg $ foldr (\x y -> fst $ dropFirst x y True) 
			world $  map KeyChar $ S.toList $ chars world

addInvs :: Inv -> [(Object, Int)] -> Maybe Inv
addInvs startInv items' = (foldl (>=>) return $ map addInv items') startInv

addInv :: (Object, Int) -> Inv -> Maybe Inv
addInv (obj, cnt) list' =
	if isHere
	then Just $ M.map change list'
	else addInvWithAlphabet alphabet list' (obj,cnt)
	where
		addInvWithAlphabet :: [Char] -> Inv -> (Object, Int) -> Maybe Inv
		addInvWithAlphabet [] _ _ = Nothing
		addInvWithAlphabet alph inv' (obj', cnt') = 
			if M.member (head alph) list'
			then addInvWithAlphabet (tail alph) inv' (obj', cnt')
			else Just $ M.insert (head alph) (obj', cnt') inv' where
		isHere = M.foldl (||) False $ M.map (\(obj',_) -> obj' == obj) list'
		change (o, n) = 
			if o == obj
			then (o, n + cnt)
			else (o, n)

addIndices :: (a -> Bool) -> [a] -> [(a, Int)]
addIndices = addIndices' 0 where
	addIndices' :: Int -> (a -> Bool) -> [a] -> [(a, Int)]
	addIndices' _ _ [] = []
	addIndices' n f (x:xs) =
		if f x
		then (x, n) : addIndices' (n + 1) f xs
		else (x, -1) : addIndices' n f xs

split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split f (x:xs) =
	if f x
	then (x:a, b)
	else (a, x:b)
	where (a, b) = split f xs
	
bindFirst :: Key -> World -> (World, Bool)
bindFirst c w = rez where
	rez = 
		if c == KeyChar '-'
		then (changeMon (changeParts newPartsSpace $ remEffect mon) newWorld, True)
		else if isNothing objects
		then (maybeAddMessage "You haven't this item!" newWorld, False)
		else if isNothing maybeNewSlot || newSlot /= slot w
		then (maybeAddMessage "This objects doesn't intended to this part!" 
			newWorld, False)
		else if isExistingBindingFirst w $ fromKey c
		then (maybeAddMessage "This item is already bound to some part!"
			newWorld, False)
		else (addNeutralMessage msg $ changeMon 
			(changeParts newParts newMon) newWorld, True)
	objects = M.lookup (fromKey c) $ inv mon
	maybeNewSlot = binds obj $ kind part
	Just newSlot = maybeNewSlot
	(obj, _) = fromJust objects
	newWorld = changeAction ' ' w
	mon = getFirst w
	part = parts mon !! shift w
	change part' = 
		if idP part == idP part'
		then part' {objectKeys = changeElem (fromEnum newSlot) 
			(fromKey c) $ objectKeys part}
		else part'
	changeSpace part' =
		if idP part == idP part'
		then part' {objectKeys = changeElem (fromEnum $ slot w) ' '
			$ objectKeys part}
		else part'
	remEffect = 
		case M.lookup (objectKeys part !! fromEnum JewelrySlot) $ inv mon of
			Nothing -> id
			Just (obj',_) -> effectOff obj' $ enchantment obj'
	addEffect = 
		if isJewelry obj
		then effectOn obj $ enchantment obj
		else id
	newMon = remEffect $ addEffect mon
	newParts = map change $ parts mon
	newPartsSpace = map changeSpace $ parts mon
	msg = name mon ++ " begin" ++ ending w 
		++ "to use " ++ title obj ++ "!"
	
bindMon :: Slot -> Char -> Int -> World -> World
bindMon sl c ind w = fst $ bindFirst (KeyChar c) $ w {shift = ind, slot = sl}

binds :: Object -> Int -> Maybe Slot
binds obj knd = 
	if isWeapon obj && knd == aRM || isLauncher obj && knd == aRM
	then Just WeaponSlot
	else if isArmor obj && knd == bind obj
	then Just ArmorSlot
	else if isJewelry obj && knd == bind obj
	then Just JewelrySlot
	else Nothing

