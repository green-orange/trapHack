module Utils.HealDamage where

import Data.Define
import Data.World
import Utils.Changes
import Monsters.Parts

import System.Random (StdGen, randomR)

doSmthByFunc :: (a -> Part -> Part) -> (Part -> Bool) -> a -> Monster -> Monster
doSmthByFunc doSmth f hp' mon = changeParts (map filterHeal $ parts mon) mon where
	filterHeal part = 
		if f part
		then doSmth hp' part
		else part
		
doSmthParts :: (a -> Part -> Part) -> Int -> a -> Monster -> Monster
doSmthParts doSmth knd = doSmthByFunc doSmth (\x -> kind x == knd)

doSmthPartById :: (a -> Part -> Part) -> Int -> a -> Monster -> Monster
doSmthPartById doSmth id' = doSmthByFunc doSmth (\x -> idP x == id')

doSmthAll :: (a -> Part -> Part) -> a -> Monster -> Monster
doSmthAll doSmth = doSmthByFunc doSmth $ const True

heal :: Int -> Part -> Part
heal n part = changeHP (min (maxhp part) $ hp part + n) part

healParts, healPartById :: Int -> Int -> Monster -> Monster
healLimbs :: Int -> Monster -> Monster
healParts = doSmthParts heal
healPartById = doSmthPartById heal
healLimbs = doSmthByFunc heal (\p -> isLowerLimb p || isUpperLimb p)

healAll :: Int -> Monster -> Monster
healAll = doSmthAll heal

dmgParts, dmgPartById :: Int -> Maybe Int -> Monster -> Monster
dmgParts a b mon = doSmthParts (dmg mon) a b mon
dmgPartById a b mon = doSmthPartById (dmg mon) a b mon

dmgAll :: Maybe Int -> Monster -> Monster
dmgAll n mon = doSmthAll (dmg mon) n mon

dmgRandom :: Maybe Int -> Monster -> StdGen -> (Monster, StdGen)
dmgRandom mbDmg mon g = (dmgPartById idNew mbDmg mon, g') where
	(n, g') = randomR (0, length (parts mon) - 1) g
	idNew = idP $ parts mon !! n
	
dmgRandomElem :: Elem -> Maybe Int -> Monster -> StdGen -> (Monster, StdGen)
dmgRandomElem elem' mbDmg mon g = (dmgElemPartById elem' idNew mbDmg mon, g') where
	(n, g') = randomR (0, length (parts mon) - 1) g
	idNew = idP $ parts mon !! n

dmgElemPartById :: Elem -> Int -> Maybe Int -> Monster -> Monster
dmgElemPartById elem' a b mon = doSmthPartById (elemDmg elem' mon) a b mon

dmg :: Monster -> Maybe Int -> Part -> Part
dmg _ Nothing part = part
dmg mon (Just n) part = part {hp =
	if hp part <= n'
	then 0
	else hp part - n'
} where n' = max 1 $ n - 2 * acPart mon part

elemDmg :: Elem -> Monster -> Maybe Int -> Part -> Part
elemDmg _ _ Nothing part = part
elemDmg elem' mon (Just n) part = part {hp =
	if hp part <= n'
	then 0
	else hp part - n'
} where n' = max 1 $ n - res mon !! fromEnum elem'

dmgFallFirst :: Int -> World -> World
dmgFallFirst hei w =
	if hei <= 0 then w
	else changeMon newMon w where
		newMon = dmgAll (Just $ 2 * hei) $ getFirst w
