module Utils4mon where

import Data
import Changes
import HealDamage
import Parts
import Messages

isSoldier :: Monster -> Bool
isSoldier mon = case name mon of
	"You"               -> False
	"Homunculus"        -> True
	"Beetle"            -> True
	"Bat"               -> False
	"Hunter"            -> True
	"Ivy"               -> False
	"Dummy"             -> False
	"Garbage collector" -> False
	"Accelerator"       -> True
	"Troll"             -> True
	"Rock"              -> False
	"Tail"              -> False
	"Worm"              -> True
	"Golem"             -> False
	"Floating eye"      -> True
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
msgCleanParts mon = foldr (:) [] $ filter ((/="") . fst) $ map (\x -> (lostMsg (name mon) 
	$ partToStr $ kind x, color)) $ filter (not . aliveP) $ parts mon where
	color = 
		if name mon == "You"
		then rEDiNVERSE
		else gREEN

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
		
killFirst :: World -> World
killFirst w = changeMon mon w where
	mon = (getFirst w) {parts = [getMain 0 0 0]}
	
canWalk :: Monster -> Bool
canWalk m = notElem (name m) ["Rock", "Ivy", "Tail", "Worm", "Dummy", "Golem"]
	
