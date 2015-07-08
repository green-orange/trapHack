module AIrepr where

import Data

getPureAI :: AIpure -> AIrepr
getPureAI aip = AIrepr {
	mods = [],
	attackIfCloseMode = Nothing,
	aipure = aip
}

humanMods, hunterMods :: [AImod]
humanMods = [HealAI, ZapAttackAI, BindArmorAI, WieldWeaponAI, UseItemsAI, PickAI]
hunterMods = [WieldLauncherAI, FireAI] ++ humanMods

getAIByList :: [AImod] -> AIpure -> AIrepr
getAIByList ms aip = AIrepr {
	mods = ms,
	attackIfCloseMode = Nothing,
	aipure = aip
}

getHumanoidAI, getHunterAI :: AIpure -> AIrepr
getHumanoidAI = getAIByList humanMods
getHunterAI =  getAIByList hunterMods

getDragonAI :: Elem -> Int -> AIrepr
getDragonAI e d = AIrepr {
	mods = [],
	attackIfCloseMode = Just (e, d),
	aipure = StupidAI
}
