module Parts where

import Data

bODY = 0 :: Int
hEAD = 1 :: Int
lEG  = 2 :: Int
aRM  = 3 :: Int
wING = 4 :: Int
pAW  = 5 :: Int
kINDS = pAW

mAIN = 32 :: Int
	
partToStr :: Int -> String
partToStr x
	| x == bODY = "Body"
	| x == hEAD = "Head"
	| x == lEG  = "Leg"
	| x == aRM  = "Arm"
	| x == wING = "Wing"
	| x == pAW  = "Paw"
	| x == mAIN = "Main"
	| otherwise = error "unknown part"

isLowerLimb :: Part -> Bool
isLowerLimb p = (kind p == lEG) || (kind p == wING) || (kind p == pAW)

isUpperLimb :: Part -> Bool
isUpperLimb p = (kind p == aRM) || (kind p == wING) || 
	(kind p == pAW) || (kind p == mAIN)

getPart :: Int -> Int -> Int -> Int -> Part
getPart knd regVel hp id = Part {
	hp = hp,
	maxhp = hp,
	kind = knd,
	idP = id,
	regVel = regVel
}

aliveP :: Part -> Bool
aliveP p = hp p > 0

getBody = getPart bODY
getHead = getPart hEAD
getLeg  = getPart lEG
getArm  = getPart aRM
getWing = getPart wING
getPaw  = getPart pAW
getMain = getPart mAIN

effectiveSlowness :: Monster -> Int
effectiveSlowness mon =
	div (slowness mon) $ 1 + (length $ filter isLowerLimb $ parts mon)
