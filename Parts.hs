module Parts where

import Data

bODY, hEAD, lEG, aRM, wING, pAW, kINDS, mAIN :: Int

bODY = 0
hEAD = 1
lEG  = 2
aRM  = 3
wING = 4
pAW  = 5
kINDS = pAW

mAIN = 32
	
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
getPart knd regVel' hp' id' = Part {
	hp = hp',
	maxhp = hp',
	kind = knd,
	idP = id',
	regVel = regVel'
}

aliveP :: Part -> Bool
aliveP p = hp p > 0

getBody, getHead, getLeg, getArm, getWing, getPaw, getMain :: 
	Int -> Int -> Int -> Part
getBody = getPart bODY
getHead = getPart hEAD
getLeg  = getPart lEG
getArm  = getPart aRM
getWing = getPart wING
getPaw  = getPart pAW
getMain = getPart mAIN

effectiveSlowness :: Monster -> Int
effectiveSlowness mon =
	max 10 $ div (slowness mon) $ 1 + (length $ filter isLowerLimb $ parts mon)
