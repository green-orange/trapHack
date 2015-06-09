module Utils4mon where

import Data
import Utils4all
import Utils4stuff
import Changes

isSoldier :: Monster -> Bool
isSoldier mon = case name mon of
	"You"        -> False
	"Homunculus" -> True
	"Beetle"     -> True
	"Bat"        -> False
	"Hunter"     -> True
	"Ivy"        -> False
	_ -> error "unknown monster"

alive :: Monster -> Bool
alive mon = hasPart bODY mon && hasPart hEAD mon || hasPart mAIN mon

countPart :: Int -> Monster -> Int
countPart knd = countPartByPred (\x -> kind x == knd)

countPartByPred :: (Part -> Bool) -> Monster -> Int
countPartByPred f mon = 
	length $ filter f $ parts mon

countUpperLimbs = countPartByPred isUpperLimb

hasPart :: Int -> Monster -> Bool
hasPart knd mon = 
	not $ null $ filter (\x -> kind x == knd) $ parts mon
	
regPart :: Part -> Part
regPart part = heal (regVel part) part

regMonster :: Monster -> Monster
regMonster mon = changeParts (map regPart $ parts mon) mon

regFirst :: World -> World
regFirst w =
	if (time $ getFirst w) == 0
	then changeMon (regMonster $ getFirst w) w
	else w

msgCleanParts :: Monster -> [(String, Int)]
msgCleanParts mon = foldr (:) [] $ map (\x -> (lostMsg (name mon) 
	$ partToStr $ kind x, color)) $ filter (not . aliveP) $ parts mon where
	color = 
		if name mon == "You"
		then rEDiNVERSE
		else gREEN

lostMsg :: String -> String -> String
lostMsg monName partName =
	if partName == "Main"
	then ""
	else monName ++ " lost " ++ addArticle partName ++ "."

isFlying :: Monster -> Bool
isFlying mon = hasPart wING mon

actTrapFirst :: World -> World
actTrapFirst w =
	if time mon == 0
	then addMessage (newMsg, rED) $ changeGen g $ changeMon newMon w
	else w
	where
	(x, y, mon) = head $ units w
	trap = worldmap w !! x !! y
	(newMon, g) = 
		if trap == fIRETRAP
		then dmgRandom (Just 8) mon $ stdgen w
		else (mon, stdgen w)
	newMsg = 
		if trap == fIRETRAP
		then
			if name mon == "You"
			then "You are in fire!"
			else name mon ++ " is in fire!"
		else ""
		
		
		
