module Utils4stuff where

import Data
import Changes
import Utils4all
import Random

import System.Random (StdGen, randomR)

doSmthByFunc :: (a -> Part -> Part) -> (Part -> Bool) -> a -> Monster -> Monster
doSmthByFunc doSmth f hp mon = changeParts mon $ map filterHeal $ parts mon where
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

cleanParts :: Monster -> Monster
cleanParts mon = changeParts mon $ filter aliveP $ parts mon

addArticle :: String -> String
addArticle str = 
	if str == ""
	then ""
	else if (elem (head str) "aeiouAEIOU")
	then "an " ++ str
	else "a " ++ str

upgrade :: Int -> Part -> Part
upgrade n part = Part {
	hp = hp part + n,
	maxhp = maxhp part + n,
	aliveP = True,
	kind = kind part,
	regVel = regVel part + 1,
	idP = idP part
}

upgradeParts = doSmthParts upgrade
upgradePartById = doSmthPartById upgrade
upgradeAll = doSmthAll upgrade

addRandomPart :: (Monster, StdGen) -> (Monster, StdGen)
addRandomPart (m, g) = (addPart m knd hp regVel, g3) where
	(knd, g1) = randomR (0, kINDS) g
	(p, g2) = randomR (0.0, 1.0) g1
	hp = 5 * inverseSquareRandom p
	(regVel, g3) = randomR (1, 4) g2

addPart :: Monster -> Int -> Int -> Int -> Monster
addPart mon knd hp regVel = changeParts mon $ newPart : parts mon where
	newPart = Part {
		hp = hp,
		maxhp = hp,
		kind = knd,
		idP = newID,
		regVel = regVel,
		aliveP = True
	}
	newID = (+) 1 $ maximum $ map idP $ parts mon


