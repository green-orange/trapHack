module ObjectOverall where

import Data
import Utils4all
import Changes
import Utils4mon

import Control.Monad ((>=>))
import Data.Set (member, empty, size)
import UI.HSCurses.Curses (Key (..))
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)

dropFirst :: Key -> World -> Bool -> (World, Bool)
dropFirst c world ignoreMessages = rez where
	objects = M.lookup (fromKey c) $ inv $ getFirst world
	rez =
		if (isNothing objects)
		then (maybeAddMessage "You haven't this item!" 
			$ changeAction ' ' world, False)
		else if c == KeyChar (weapon $ getFirst world) && (alive $ getFirst world)
		then (maybeAddMessage "You can't drop weapon that you wield!" 
			$ changeAction ' ' world, False)
		else (changeMon mon $ addNeutralMessage newMsg $ addItem (x, y, obj, cnt) 
			$ changeAction ' ' world, True)
	(obj, cnt) = fromJust objects
	(x, y, oldmon) = head $ units world
	mon = delAllObj c $ oldmon
	newMsg =
		if ignoreMessages
		then ""
		else (name $ getFirst world) ++ " drop" ++ ending world ++ titleShow obj ++ "."

dropAll :: World -> World
dropAll world = foldr (\x y -> fst $ dropFirst x y True) world $ 
	map (KeyChar . fst) $ M.toList $ inv $ getFirst world

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

addInvs :: Inv -> [(Object, Int)] -> Maybe Inv
addInvs startInv items = (foldl (>=>) return $ map addInv items) startInv

addInv :: (Object, Int) -> Inv -> Maybe Inv
addInv (obj, cnt) list =
	if isHere
	then Just $ M.map change list 
	else addInvWithAlphabet alphabet list (obj,cnt)
	where
		addInvWithAlphabet :: [Char] -> Inv -> (Object, Int) -> Maybe Inv
		addInvWithAlphabet [] _ _ = Nothing
		addInvWithAlphabet alph inv (obj, cnt) = 
			if M.member (head alph) list
			then addInvWithAlphabet (tail alph) inv (obj, cnt)
			else Just $ M.insert (head alph) (obj, cnt) inv where
		isHere = M.foldl (||) False $ M.map (\(obj',_) -> obj' == obj) list
		change (o, n) = 
			if o == obj
			then (o, n + cnt)
			else (o, n)
