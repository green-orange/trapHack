module Items.ItemsOverall where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Monsters
import Utils.Items
import IO.Messages
import IO.Colors
import IO.Texts

import Control.Monad ((>=>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.Functor ((<$>))

-- | drop an item with given position in inventory;
-- ignore messages if third argument is True
dropFirst :: Char -> World -> Bool -> (World, Bool)
dropFirst c world ignoreMessages =
	case objects of
		Nothing -> (maybeAddMessage msgNoItem world {action = Move}, False)
		Just (obj, cnt) ->
			if isExistingBindingFirst world c && alive (getFirst world)
			then (maybeAddMessage msgDropEquipped world {action = Move}, False)
			else let
				newMsg =
					if ignoreMessages
					then ""
					else name (getFirst world) ++ " drop" ++ ending world 
						++ titleShow obj ++ "."
			in (changeMon mon $ addNeutralMessage newMsg
				$ addItem (x, y, obj, cnt) world {action = Move}, True)
	where
		objects = M.lookup c $ inv $ getFirst world
		x = xFirst world
		y = yFirst world
		oldmon = getFirst world
		mon = delAllObj c oldmon

-- | drop all items in the inventory of current monster
dropAll :: World -> World
dropAll world = foldr ((\x y -> fst $ dropFirst x y True) . fst)
	world $ M.toList $ inv $ getFirst world

-- | pick items in the current 'chars' world
pickFirst :: World -> (Maybe World, String)
pickFirst world =
	if S.null $ chars world
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
			&& S.member (alphabet !! n) (chars world)) itemsWithIndices
		newItems = fst <$> rest
		maybeInv = addInvs (inv oldMon) $ ((\(_,_,a,b) 
			-> (a,b)) . fst) <$> itemsToPick
	in case maybeInv of
	Nothing -> (Nothing, msgFullInv)
	Just newInv ->
		let
		mon = oldMon {inv = newInv}
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
			action = Move,
			chars = S.empty
		}, "")

-- | drop items in the current 'chars' world
dropManyFirst :: World -> Maybe World
dropManyFirst world =
	if S.null $ chars world
	then Nothing
	else Just newWorld where
		newMsg = name (getFirst world) ++ " drop" ++ ending world 
			++ "some objects."
		newWorld = 
			addNeutralMessage newMsg $ foldr (\ x y -> fst $ dropFirst
				x y True) world {action = Move, chars = S.empty} 
				$ S.toList $ chars world

-- | add list of items with counts to given inventory if it's possible
addInvs :: Inv -> [(Object, Int)] -> Maybe Inv
addInvs startInv items' = foldr ((>=>) . addInv) return items' startInv

-- | add one item with count to given inventory if it's possible
addInv :: (Object, Int) -> Inv -> Maybe Inv
addInv (obj, cnt) list' =
	if isHere
	then Just $ change <$> list'
	else addInvWithAlphabet alphabet list' (obj,cnt)
	where
		isHere = M.foldr (||) False $ (\(obj',_) -> obj' == obj) <$> list'
		change (o, n) = (o, if o == obj then n + cnt else n)

-- | add one item with count and given alphabet if it's possible 
addInvWithAlphabet :: String -> Inv -> (Object, Int) -> Maybe Inv
addInvWithAlphabet [] _ _ = Nothing
addInvWithAlphabet (a:as) inv' (obj', cnt') = 
	if M.member a inv'
	then addInvWithAlphabet as inv' (obj', cnt')
	else Just $ M.insert a (obj', cnt') inv'

-- | converts [x_i] to [(x_i, a_i)] where a_i == -1 if not (f x_i);
-- else a_i are [0..]
addIndices :: (a -> Bool) -> [a] -> [(a, Int)]
addIndices = addIndices' 0 where
	addIndices' :: Int -> (a -> Bool) -> [a] -> [(a, Int)]
	addIndices' _ _ [] = []
	addIndices' n f (x:xs) =
		if f x
		then (x, n) : addIndices' (n + 1) f xs
		else (x, -1) : addIndices' n f xs

-- | split f xs === (filter f xs, filter (not . f) xs)
split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split f (x:xs) =
	if f x
	then (x:a, b)
	else (a, x:b)
	where (a, b) = split f xs

-- | binds item with given position in the inventory to a changed body part
bindFirst :: Char -> World -> (World, Bool)
bindFirst c w =
	if c == '-'
	then (changeMon (remEffect mon {parts = newPartsSpace}) newWorld, True)
	else case objects of
		Nothing -> (maybeAddMessage msgNoItem newWorld, False)
		Just (obj, _) -> let
			maybeNewSlot = binds obj $ kind part
			addEffect = 
				if isJewelry obj
				then effectOn obj $ enchantment obj
				else id				
			msg = name mon ++ " begin" ++ ending w 
				++ "to use " ++ title obj ++ "!"
			newMon = remEffect $ addEffect mon
			in case maybeNewSlot of 
			Nothing -> (maybeAddMessage msgWrongBind newWorld, False)
			Just newSlot -> 
				let
				change part' = 
					if idP part == idP part'
					then part' {objectKeys = changeElem (fromEnum newSlot) 
						c $ objectKeys part}
					else part'	
				newParts = change <$> parts mon
				in
				if newSlot /= slot w
				then (maybeAddMessage msgWrongBind newWorld, False)
				else if isExistingBindingFirst w c
				then (maybeAddMessage msgRepeatedBind newWorld, False)
				else (addNeutralMessage msg $ changeMon 
					(newMon {parts = newParts}) newWorld, True)
	where
		objects = M.lookup c $ inv mon
		newWorld = w {action = Move}
		mon = getFirst w
		part = parts mon !! shift w
		changeSpace part' =
			if idP part == idP part'
			then part' {objectKeys = changeElem (fromEnum $ slot w) ' '
				$ objectKeys part}
			else part'
		remEffect = 
			case M.lookup (objectKeys part !! fromEnum JewelrySlot) $ inv mon of
				Nothing -> id
				Just (obj',_) -> effectOff obj' $ enchantment obj'
		newPartsSpace = changeSpace <$> parts mon

-- | binds an item with index 'c' to slot 'sl' and part 'ind'
bindMon :: Slot -> Char -> Int -> World -> World
bindMon sl c ind w = fst $ bindFirst c $ w {shift = ind, slot = sl}

-- | return a slot to bind given object with given kind of body part,
-- or Nothing if it doesn't exist 
binds :: Object -> Int -> Maybe Slot
binds obj knd
	| (isWeapon obj || isLauncher obj) && knd == aRM = 
		Just WeaponSlot
	| isArmor obj && knd == bind obj = Just ArmorSlot
	| isJewelry obj && knd == bind obj = Just JewelrySlot
	| otherwise = Nothing

-- | read decimal number by chars
addNumber :: Char -> World -> World
addNumber c w = 
	if isDigit c
	then w {numToSplit = numToSplit w * 10 + fromEnum c - fromEnum '0'}
	else addDefaultMessage msgNaN w

-- | split stack of items where index is 'prevAction' and count is 'numToSplit'
splitFirst :: World -> World
splitFirst w =
	case maybeObj of
		Nothing -> maybeAddMessage msgNoItem w
		Just (obj, n) -> case addInvWithAlphabet alphabet
			(M.insert (prevAction w) (obj, n - pile) $ inv mon) (obj, pile) of
				Nothing -> maybeAddMessage msgFullInv w
				Just newInv
					| pile > n -> maybeAddMessage msgNotEnough w
					| pile == n || pile == 0 -> w
					| otherwise -> changeMon (mon {inv = newInv}) w
	where
		mon = getFirst w
		pile = numToSplit w
		maybeObj = M.lookup (prevAction w) $ inv mon
	
