module Utils4objects where

import DataWorld
import DataDef

isPotion :: Object -> Bool
isPotion (Potion {}) = True
isPotion _ = False

isScroll :: Object -> Bool
isScroll (Scroll {}) = True
isScroll _ = False

isWand :: Object -> Bool
isWand (Wand {}) = True
isWand _ = False

isTrap :: Object -> Bool
isTrap (Trap {}) = True
isTrap _ = False

isLauncher :: Object -> Bool
isLauncher (Launcher {}) = True
isLauncher _ = False

isWeapon :: Object -> Bool
isWeapon (Weapon {}) = True
isWeapon _ = False

isMissile :: Object -> Bool
isMissile (Missile {}) = True
isMissile _ = False

isArmor :: Object -> Bool
isArmor (Armor {}) = True
isArmor _ = False

isJewelry :: Object -> Bool
isJewelry (Jewelry {}) = True
isJewelry _ = False

isExistingBinding :: Monster -> Char -> Bool
isExistingBinding mon c = elem c $ concat $ map objectKeys $ parts mon

isExistingBindingFirst :: World -> Char -> Bool
isExistingBindingFirst = isExistingBinding . getFirst
