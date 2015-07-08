module Parts where

import Data
import Utils4objects
import DataObject
import DataDef

import qualified Data.Map as M

baseEncumbrance :: Int
baseEncumbrance = 500

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
	regVel = regVel',
	objectKeys = replicate sLOTS ' '
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

encumbrance :: Monster -> Int
encumbrance mon = M.foldr (+) 0 $ M.map (\(o, n) -> n * weight o) $ inv mon

effectiveSlowness :: Monster -> Int
effectiveSlowness mon = max 10 $ (`div` baseEncumbrance) $ 
	(*) (max baseEncumbrance $ encumbrance mon) $ div (slowness mon) 
	$ 1 + length (filter isLowerLimb $ parts mon)

isEmptyPart :: Slot -> Monster -> Part -> Bool
isEmptyPart sl mon part = M.notMember (objectKeys part !! fromEnum sl) $ inv mon

acPart :: Monster -> Part -> Int
acPart mon part = (case armor of
	Nothing -> 0
	Just (obj, _) -> if isArmor obj then ac obj else 0) +
	(case jewelry of
	Nothing -> 0
	Just (obj,_) -> 
		if title obj == "ring of protection" then enchantment obj else 0)
	where
		armor = M.lookup (objectKeys part !! fromEnum ArmorSlot) (inv mon)
		jewelry = M.lookup (objectKeys part !! fromEnum JewelrySlot) (inv mon)

