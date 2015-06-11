module ObjectOverall where

import Data
import Utils4all
import Changes
import Utils4mon

import Control.Monad ((>=>))
import Data.Set (member, empty, size)
import UI.HSCurses.Curses (Key (..))

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
	mon = delAllObj c $ oldmon
	newMsg =
		if ignoreMessages
		then ""
		else (name $ getFirst world) ++ " drop" ++ ending world ++ titleShow obj ++ "."

dropAll :: World -> World
dropAll world = foldr (\x y -> fst $ dropFirst x y True) world $ 
	map (KeyChar . first) $ inv $ getFirst world

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
