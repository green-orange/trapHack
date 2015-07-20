module Items.Craft where

import Data.Define
import Data.World
import Utils.Items
import Utils.Changes
import Items.Stuff
import Items.ItemsOverall
import IO.Messages
import IO.Texts

import qualified Data.Map as M

recipes :: [Recipe]
recipes = [recWoodenSword, recStoneSword]

remRes :: ResourceType -> Inv -> Maybe Inv
remRes rt inv'
	| null ress = Nothing
	| n == 1 = Just $ M.delete c inv'
	| otherwise = Just $ M.insert c (obj, n - 1) inv'
	where
	ress = M.toList $ M.filter (\(o, _) -> isResource o 
		&& restype o == rt) inv'
	(c, (obj, n)):_ = ress

remRess :: ResourceType -> Int -> Inv -> Maybe Inv
remRess rt cnt inv' = foldr (=<<) (Just inv') $ replicate cnt $ remRes rt

craftByChar :: Char -> World -> (World, Bool)
craftByChar c w = 
	if ind >= 0 && ind < length recipes
	then maybeRunRecipe (recipes !! ind) w {action = Move}
	else (maybeAddMessage msgUnkRep w {action = Move}, False)
	where ind = fromEnum c - fromEnum 'a'

maybeRunRecipe :: Recipe -> World -> (World, Bool)
maybeRunRecipe (ress, rez) w =
	case foldr ((=<<) . uncurry remRess) (Just $ inv $ getFirst w) ress of
		Nothing -> (maybeAddMessage msgNotEnough w, False)
		Just delInv -> case addInv (rez, 1) delInv of
			Nothing -> (maybeAddMessage msgFullInv w, False)
			Just inv' -> (addNeutralMessage newMsg 
				$ changeMon ((getFirst w) {inv = inv'}) w, True) where
				newMsg = msgCraft (name $ getFirst w) $ title rez

recWoodenSword :: Recipe
recWoodenSword = ([(Tree, 3)], woodenSword)

recStoneSword :: Recipe
recStoneSword = ([(Stone, 2), (Tree, 1)], stoneSword)
