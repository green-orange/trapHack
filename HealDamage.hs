module HealDamage where

import Data
import Changes

import System.Random (StdGen, randomR)

doSmthByFunc :: (a -> Part -> Part) -> (Part -> Bool) -> a -> Monster -> Monster
doSmthByFunc doSmth f hp mon = changeParts (map filterHeal $ parts mon) mon where
	filterHeal part = 
		if f part
		then doSmth hp part
		else part
		
doSmthParts :: (a -> Part -> Part) -> Int -> a -> Monster -> Monster
doSmthParts doSmth knd = doSmthByFunc doSmth (\x -> kind x == knd)

doSmthPartById :: (a -> Part -> Part) -> Int -> a -> Monster -> Monster
doSmthPartById doSmth id = doSmthByFunc doSmth (\x -> idP x == id)

doSmthAll :: (a -> Part -> Part) -> a -> Monster -> Monster
doSmthAll doSmth = doSmthByFunc doSmth $ const True

heal :: Int -> Part -> Part
heal n part = changeHP (min (maxhp part) $ hp part + n) part

healParts = doSmthParts heal
healPartById = doSmthPartById heal
healAll = doSmthAll heal

dmgParts = doSmthParts dmg
dmgPartById = doSmthPartById dmg
dmgAll = doSmthAll dmg

dmgRandom :: Maybe Int -> Monster -> StdGen -> (Monster, StdGen)
dmgRandom mbDmg mon g = (dmgPartById idNew mbDmg mon, g') where
	(n, g') = randomR (0, (length $ parts mon) - 1) g
	idNew = idP $ parts mon !! n

dmg :: Maybe Int -> Part -> Part
dmg Nothing part = part
dmg (Just n) part = Part {
	hp =
		if die
		then 0
		else hp part - n,
	maxhp = maxhp part,
	aliveP = not die,
	kind = kind part,
	regVel = regVel part,
	idP = idP part
} where die = hp part <= n

