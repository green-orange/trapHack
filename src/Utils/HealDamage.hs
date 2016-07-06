module Utils.HealDamage where

import Data.Define
import Data.World
import Utils.Changes
import Monsters.Parts

import System.Random (StdGen, randomR)
import Data.Functor ((<$>))

-- | do some parametrized action with parts filtered by some predicate 
doSmthByFunc :: (a -> Part -> Part) -> (Part -> Bool) -> a -> Monster -> Monster
doSmthByFunc doSmth f hp' mon = mon {parts = filterHeal <$> parts mon} where
	filterHeal part = 
		if f part
		then doSmth hp' part
		else part

-- | do smth with parts of given kind
doSmthParts :: (a -> Part -> Part) -> PartKind -> a -> Monster -> Monster
doSmthParts doSmth knd = doSmthByFunc doSmth ((== knd) . kind)

-- | do smth with one part with given id
doSmthPartById :: (a -> Part -> Part) -> Int -> a -> Monster -> Monster
doSmthPartById doSmth id' = doSmthByFunc doSmth ((== id') . idP)

-- | do smth with all parts
doSmthAll :: (a -> Part -> Part) -> a -> Monster -> Monster
doSmthAll doSmth = doSmthByFunc doSmth $ const True

-- | heal given part
heal :: Int -> Part -> Part
heal n part = part {hp = min (maxhp part) $ hp part + n}

healParts :: PartKind -> Int -> Monster -> Monster
healPartById :: Int -> Int -> Monster -> Monster
healLimbs :: Int -> Monster -> Monster
-- | heal parts with given kind
healParts = doSmthParts heal
-- | heal parts with given id
healPartById = doSmthPartById heal
-- | heal all limbs
healLimbs = doSmthByFunc heal (isLowerLimb ||| isUpperLimb)

-- | heal all parts
healAll :: Int -> Monster -> Monster
healAll = doSmthAll heal

dmgParts :: PartKind -> Maybe Int -> Monster -> Monster
dmgPartById :: Int -> Maybe Int -> Monster -> Monster
-- | damage parts with given kind
dmgParts a b mon = doSmthParts (dmg mon) a b mon
-- | damage part with given id
dmgPartById a b mon = doSmthPartById (dmg mon) a b mon

-- | damage all parts
dmgAll :: Maybe Int -> Monster -> Monster
dmgAll n mon = doSmthAll (dmg mon) n mon

-- | damage random part
dmgRandom :: Maybe Int -> Monster -> StdGen -> (Monster, StdGen)
dmgRandom mbDmg mon g = (dmgPartById idNew mbDmg mon, g') where
	(n, g') = randomR (0, length (parts mon) - 1) g
	idNew = idP $ parts mon !! n

-- | damage random part by elemental attack
dmgRandomElem :: Elem -> Maybe Int -> Monster -> StdGen -> (Monster, StdGen)
dmgRandomElem elem' mbDmg mon g = (dmgElemPartById elem' idNew mbDmg mon, g') where
	(n, g') = randomR (0, length (parts mon) - 1) g
	idNew = idP $ parts mon !! n

-- | damage part with given id by elemental attack
dmgElemPartById :: Elem -> Int -> Maybe Int -> Monster -> Monster
dmgElemPartById elem' a b mon = doSmthPartById (elemDmg elem' mon) a b mon

-- | damage given part
dmg :: Monster -> Maybe Int -> Part -> Part
dmg _ Nothing part = part
dmg mon (Just n) part = part {hp =
	if hp part <= n'
	then 0
	else hp part - n'
} where n' = max 1 $ n - 2 * acPart mon part

-- | damage given part by elemental attack
elemDmg :: Elem -> Monster -> Maybe Int -> Part -> Part
elemDmg _ _ Nothing part = part
elemDmg elem' mon (Just n) part = part {hp =
	if hp part <= n'
	then 0
	else hp part - n'
} where n' = max 1 $ n - res mon !! fromEnum elem'

-- | damage monster by falling
dmgFallFirst :: Int -> World -> World
dmgFallFirst hei w =
	if isFlying (getFirst w) || hei <= 1 then w
	else changeMon newMon w where
		newMon = dmgAll (Just $ 3 * hei) $ getFirst w
