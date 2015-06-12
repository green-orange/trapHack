module Parts where

import Data
import Utils4all

getPart :: Int -> Int -> Int -> Int -> Part
getPart knd regVel hp id = Part {
	hp = hp,
	maxhp = hp,
	kind = knd,
	idP = id,
	regVel = regVel,
	aliveP = True
}

getBody = getPart bODY
getHead = getPart hEAD
getLeg  = getPart lEG
getArm  = getPart aRM
getWing = getPart wING
getPaw  = getPart pAW
getMain = getPart mAIN
