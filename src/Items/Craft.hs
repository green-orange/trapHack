module Items.Craft where

import Data.Define
import Data.World
import Utils.Items
import Utils.Changes
import Items.Stuff
import Items.ItemsOverall
import IO.Messages
import IO.Texts
import IO.Colors

import qualified Data.Map as M
import Data.Maybe (isJust)

-- | remove given resource from inventory if it's possible
-- else return Nothing
remRes :: ResourceType -> Inv -> Maybe Inv
remRes rt inv'
	| null ress = Nothing
	| n == 1 = Just $ M.delete c inv'
	| otherwise = Just $ M.insert c (obj, n - 1) inv'
	where
	ress = M.toList $ M.filter (\(o, _) -> isResource o 
		&& restype o == rt) inv'
	(c, (obj, n)):_ = ress

-- | remove 'cnt' resources from inventory if it's possible
remRess :: ResourceType -> Int -> Inv -> Maybe Inv
remRess rt cnt inv' = foldr (=<<) (Just inv') $ replicate cnt $ remRes rt

-- | use a recipe by a given char
craftByChar :: Char -> World -> (World, Bool)
craftByChar c w = 
	if ind >= 0 && ind < length recipes
	then maybeRunRecipe (recipes !! ind) w {action = Move}
	else (maybeAddMessage msgUnkRep w {action = Move}, False)
	where ind = fromEnum c - fromEnum 'a'

-- | use given recipe
maybeRunRecipe :: Recipe -> World -> (World, Bool)
maybeRunRecipe (ress, rez) w =
	case foldr ((=<<) . uncurry remRess) (Just $ inv $ getFirst w) ress of
		Nothing -> (maybeAddMessage msgNotEnough w, False)
		Just delInv -> case addInv (rez, 1) delInv of
			Nothing -> (maybeAddMessage msgFullInv w, False)
			Just inv' -> (addNeutralMessage newMsg 
				$ changeMon ((getFirst w) {inv = inv'}) w, True) where
				newMsg = msgCraft (name $ getFirst w) $ title rez

-- | list of recipes
recipes :: [Recipe]
recipes = [recWoodenSword, recStoneSword, recPickAxe]

-- | is given recipe available
isAvailableRecipe :: Inv -> [(ResourceType, Int)] -> Bool
isAvailableRecipe inv' ress = isJust $ foldr ((=<<) . uncurry remRess)
	(Just inv') ress

colorFromRecipe :: Inv -> [(ResourceType, Int)] -> Int
colorFromRecipe inv' ress = if ok then gREEN else dEFAULT where
	ok = isAvailableRecipe inv' ress

recWoodenSword, recStoneSword, recPickAxe :: Recipe
recWoodenSword = ([(Tree, 3)], woodenSword)
recStoneSword = ([(Stone, 2), (Tree, 1)], stoneSword)
recPickAxe = ([(Stone, 3), (Tree, 2)], pickAxe)
