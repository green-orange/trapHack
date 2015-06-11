module Utils4stuff where

import Data
import Changes
import Utils4all hiding (split)
import Random
import Move (stupidestAI)
import HealDamage

import System.Random (StdGen, randomR, split)

cleanParts :: Monster -> Monster
cleanParts mon = changeParts (filter aliveP $ parts mon) mon

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
addPart mon knd hp regVel = changeParts (newPart : parts mon) mon where
	newPart = Part {
		hp = hp,
		maxhp = hp,
		kind = knd,
		idP = newID,
		regVel = regVel,
		aliveP = True
	}
	newID = (+) 1 $ maximum $ map idP $ parts mon

fireAround :: Int -> (Int, Int) -> World -> World
fireAround d pair w = addMessages newMsgs $ changeGen g $ changeMons newMons w where
	(xNow, yNow, _) = head $ units w
	(newDmg, g) = randomR pair $ stdgen w
	newMons = zipWith fireDmg (units w) (infgens g)
	infgens :: StdGen -> [StdGen]
	infgens g = g' : infgens g'' where
		(g', g'') = split g
	isClose (x, y, _) = abs (x - xNow) <= d && abs (y - yNow) <= d
	fireDmg arg@(x, y, mon) gen = 
		if isClose arg
		then (x, y, fst $ dmgRandom (Just newDmg) mon gen)
		else arg
	msg (_,_,mon) = 
		if name mon == "You"
		then ("You are in fire!", rED)
		else (name mon ++ " is in fire!", gREEN)
	newMsgs = map msg $ filter isClose $ units w
	
stupidity :: Monster -> Monster
stupidity mon = mon {ai = newAI} where
	newAI = case ai mon of
		You -> You
		AI _ -> AI stupidestAI
	
isUntrappable :: Terrain -> Bool
isUntrappable = (/=) eMPTY



