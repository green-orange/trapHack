module Messages where

import Data
import Changes
import Utils4objects
import Colors
import Parts
import Texts
import DataWorld
import DataMonster
import DataDef

import qualified Data.Map as M
import Data.Array

titleShow :: Object -> String
titleShow x = title x ++ 
	if isWand x
	then " (" ++ show (charge x) ++ ")"
	else if isWeapon x || isArmor x || isLauncher x || isJewelry x
	then " (" ++ (if enchantment x >= 0 then "+" else "") 
		++ show (enchantment x) ++ ")"
	else ""

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toEnum (fromEnum x - fromEnum 'a' + fromEnum 'A') : xs

ending :: World -> String
ending world =
	if isPlayerNow world
	then " "
	else "s "

addArticle :: String -> String
addArticle str
	| str == "" = ""
	| head str `elem` "aeiouAEIOU" = "an " ++ str
	| otherwise = "a " ++ str

lostMsg :: String -> String -> String
lostMsg monName partName =
	if partName == "Main"
	then ""
	else monName ++ " lost " ++ addArticle partName ++ "."
	
maybeAddMessage :: String -> World -> World
maybeAddMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, yELLOW) w
	else w
	
addNeutralMessage :: String -> World -> World
addNeutralMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, gREEN) w
	else addMessage (msg, yELLOW) w
	
addDefaultMessage :: String -> World -> World
addDefaultMessage msg = addMessage (msg, dEFAULT)

getInfo :: World -> World
getInfo w = changeAction ' ' $ 
	addDefaultMessage msg w where msg = infoMessage w

infoMessage :: World -> String
infoMessage w
	| xInfo w < 0 || yInfo w < 0 || xInfo w > maxX || yInfo w > maxY
		= msgNECell
	| abs (xInfo w - xFirst w) > xSight || abs (yInfo w - yFirst w) > ySight
		= msgUnseenCell
	| last str == ' ' = init str 
	| otherwise = str where
	x = xInfo w
	y = yInfo w
	terr = worldmap w ! (x, y)
	un = M.lookup (x, y) $ units w
	objs = filter (\(x',y',_,_) -> x' == x && y' == y) $ items w
	terrInfo = "Terrain: " ++ show terr ++ ". "
	monInfo = case un of
		Nothing -> ""
		Just mon -> "Monster: " ++ name mon ++ ". Parts: " ++ 
			concatMap (\p -> partToStr (kind p) ++ "; ") (parts mon)
	objsInfo = case objs of
		[] -> ""
		_ -> (++) "Objects: " $ concatMap (\(_,_,i,n) -> titleShow i ++ 
			(if n == 1 then "; " else " (" ++ show n ++ "); ")) objs
	str = terrInfo ++ monInfo ++ objsInfo
