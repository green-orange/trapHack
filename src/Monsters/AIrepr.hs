module Monsters.AIrepr where

import Data.Define

humanMods, hunterMods :: [AImod]
-- | modificators for all humanoids
humanMods = [HealAI, ZapAttackAI, BindArmorAI, WieldWeaponAI, UseItemsAI, 
	PickAI, EatAI]
-- | modificators for a Hunter
hunterMods = [WieldLauncherAI, FireAI] ++ humanMods

-- | get AI without range attacks 
getAIByList :: [AImod] -> AIpure -> AIrepr
getAIByList ms aip = AIrepr {
	mods = ms,
	attackIfCloseMode = Nothing,
	aipure = aip
}

getPureAI, getEatAI, getHumanoidAI, getHunterAI :: AIpure -> AIrepr
-- | get AI without range attacks and modificators
getPureAI = getAIByList []
-- | get AI with only pick and eat AI
getEatAI = getAIByList [PickAI, EatAI]
-- | get standard humanoid AU
getHumanoidAI = getAIByList humanMods
-- | get hunter AI
getHunterAI =  getAIByList hunterMods

-- | get dragon AI by element and range
getDragonAI :: Elem -> Int -> AIrepr
getDragonAI e d = AIrepr {
	mods = [EatAI],
	attackIfCloseMode = Just (e, d),
	aipure = CleverUAI
}
