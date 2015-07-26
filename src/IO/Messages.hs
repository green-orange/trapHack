module IO.Messages where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Items
import Monsters.Parts
import IO.Colors
import IO.Texts

import qualified Data.Map as M
import Data.Array

-- | return full title of an item
titleShow :: Object -> String
titleShow x = title x ++ 
	if isWand x || isTool x
	then " (" ++ show (charge x) ++ ")"
	else if isWeapon x || isArmor x || isLauncher x || isJewelry x
	then " (" ++ (if enchantment x >= 0 then "+" else "") 
		++ show (enchantment x) ++ ")"
	else ""

-- | capitalize first letter of the string if it isn't empty
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toEnum (fromEnum x - fromEnum 'a' + fromEnum 'A') : xs

-- | return " " if current monster is a player and "s " otherwise
ending :: World -> String
ending world = if isPlayerNow world then " " else "s "

-- | add "a " or "an " article to a string
addArticle :: String -> String
addArticle "" = ""
addArticle str@(x:_) =
	if x `elem` "aeiouAEIOU" then "an " ++ str
	else "a " ++ str

-- | add message about losing a body part
lostMsg :: String -> String -> String
lostMsg monName partName =
	if partName == "Main"
	then ""
	else monName ++ " lost " ++ addArticle partName ++ "."

-- | add yellow message if current monster is a player
maybeAddMessage :: String -> World -> World
maybeAddMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, yELLOW) w
	else w

-- | add green message if current monster is a player and yellow otherwise 
addNeutralMessage :: String -> World -> World
addNeutralMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, gREEN) w
	else addMessage (msg, yELLOW) w

-- | add white message
addDefaultMessage :: String -> World -> World
addDefaultMessage msg = addMessage (msg, dEFAULT)

-- | get info about chosen cell and continue the step
getInfo :: World -> World
getInfo w = addDefaultMessage msg w {action = Move} where msg = infoMessage w

-- | get string with info
infoMessage :: World -> String
infoMessage w
	| not $ isCell (xInfo w) (yInfo w) = msgNECell
	| abs (xInfo w - xFirst w) > xSight || abs (yInfo w - yFirst w) > ySight
		= msgUnseenCell
	| last str == ' ' = init str 
	| otherwise = str where
	x = xInfo w
	y = yInfo w
	Cell terr hei = worldmap w ! (x, y)
	un = M.lookup (x, y) $ units w
	objs = filter (\(x',y',_,_) -> x' == x && y' == y) $ items w
	terrInfo = "Terrain: " ++ show terr ++ ". Height: " ++ show hei ++ ". "
	monInfo = case un of
		Nothing -> ""
		Just mon -> "Monster: " ++ name mon ++ ". Parts: " ++ 
			concatMap (\p -> partToStr (kind p) ++ "; ") (parts mon)
	objsInfo = case objs of
		[] -> ""
		_ -> (++) "Objects: " $ concatMap (\(_,_,i,n) -> titleShow i ++ 
			(if n == 1 then "; " else " (" ++ show n ++ "); ")) objs
	str = terrInfo ++ monInfo ++ objsInfo
