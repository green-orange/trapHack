module Utils4mon where

import Data
import Utils4all
import Utils4stuff
import Changes

isSoldier :: Monster -> Bool
isSoldier mon = case name mon of
	"You" -> False
	"Homunculus" -> True
	"Beetle" -> True
	"Bat" -> False
	_ -> error "wrong monster"

alive :: Monster -> Bool
alive mon = hasPart bODY mon && hasPart hEAD mon

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
regMonster mon = changeParts mon $ map regPart $ parts mon

regFirst :: World -> World
regFirst w =
	if (time $ getFirst w) == 0
	then changeMon (regMonster $ getFirst w) w
	else w

msgCleanParts :: Monster -> String
msgCleanParts mon = foldl (++) [] $ map (\x ->
	lostMsg (name mon) $ partToStr $ kind x) $ filter (not . aliveP) $ parts mon

lostMsg :: String -> String -> String
lostMsg monName partName = monName ++ " lost " ++ addArticle partName ++ ". "


