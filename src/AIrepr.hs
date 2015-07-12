module AIrepr where

import DataDef

humanMods, hunterMods :: [AImod]
humanMods = [HealAI, ZapAttackAI, BindArmorAI, WieldWeaponAI, UseItemsAI, 
	PickAI, EatAI]
hunterMods = [WieldLauncherAI, FireAI] ++ humanMods

getAIByList :: [AImod] -> AIpure -> AIrepr
getAIByList ms aip = AIrepr {
	mods = ms,
	attackIfCloseMode = Nothing,
	aipure = aip
}

getPureAI, getEatAI, getHumanoidAI, getHunterAI :: AIpure -> AIrepr
getPureAI = getAIByList []
getEatAI = getAIByList [PickAI, EatAI]
getHumanoidAI = getAIByList humanMods
getHunterAI =  getAIByList hunterMods

getDragonAI :: Elem -> Int -> AIrepr
getDragonAI e d = AIrepr {
	mods = [EatAI],
	attackIfCloseMode = Just (e, d),
	aipure = StupidAI
}
