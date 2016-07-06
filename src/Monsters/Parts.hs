module Monsters.Parts where

import Data.Define
import Utils.Items

import qualified Data.Map as M
import Data.Functor ((<$>))

-- | capacity for monster with zero strength
baseCapacity :: Int
baseCapacity = 50
-- | increase in capacity per unit of strength
capacityByStrength :: Int
capacityByStrength = 40
-- | capacity of the monster
capacity :: Monster -> Int
capacity mon = baseCapacity + capacityByStrength *
	(intr mon !! fromEnum Strength)

-- | detect lower limbs to walk
isLowerLimb :: Part -> Bool
isLowerLimb p = (kind p == Leg) || (kind p == Wing) || (kind p == Paw)
-- | detect upper limbs to attack
isUpperLimb :: Part -> Bool
isUpperLimb p = (kind p == Arm) || (kind p == Wing) || 
	(kind p == Paw) || (kind p == Main)

-- | get part by given kind, regeneration rate, hp and id
getPart :: PartKind -> Int -> Int -> Int -> Part
getPart knd regRate' hp' id' = Part {
	hp = hp',
	maxhp = hp',
	kind = knd,
	idP = id',
	regRate = regRate',
	objectKeys = replicate sLOTS ' '
}

-- | check is this part alive
aliveP :: Part -> Bool
aliveP p = hp p > 0

-- | getPart specifications
getBody, getHead, getLeg, getArm, getWing, getPaw, getMain :: 
	Int -> Int -> Int -> Part
getBody = getPart Body
getHead = getPart Head
getLeg  = getPart Leg
getArm  = getPart Arm
getWing = getPart Wing
getPaw  = getPart Paw
getMain = getPart Main

-- | calculate encumbrance of the monster
encumbrance :: Monster -> Int
encumbrance mon = M.foldr (+) 0 $ (\(o, n) -> n * weight o) <$> inv mon

-- | calculate effective slowness of the monster
-- (with encumbrance effect, count of legs etc)
effectiveSlowness :: Monster -> Int
effectiveSlowness mon = max 10 $ (`div` capacity mon) $ 
	(*) (max (capacity mon) $ encumbrance mon) $ div (slowness mon) 
	$ 1 + length (filter isLowerLimb $ parts mon)

-- | check is this slot of given part empty
isEmptyPart :: Slot -> Monster -> Part -> Bool
isEmptyPart sl mon part = M.notMember (objectKeys part !! fromEnum sl) $ inv mon

-- | calculate armor class of the part armor
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

-- | have this monster any part of given kind?
hasPart :: PartKind -> Monster -> Bool
hasPart knd mon = 
	any ( (== knd) . kind) $ parts mon

-- | have this monsters upper limbs?
hasUpperLimb :: Monster -> Bool
hasUpperLimb mon = any isUpperLimb $ parts mon

-- | can this monster fly?
isFlying :: Monster -> Bool
isFlying = hasPart Wing

