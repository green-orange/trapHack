module ObjectOverall where

import Data
import Changes
import Utils4mon
import Messages

import Control.Monad ((>=>))
import qualified Data.Set as S
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
