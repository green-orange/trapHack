module Utils4objects where

import Data

import Control.Monad (join)

isPotion :: Object -> Bool
isPotion (Potion _ _) = True
isPotion _ = False

isScroll :: Object -> Bool
isScroll (Scroll _ _) = True
isScroll _ = False

isWand :: Object -> Bool
isWand (Wand _ _ _ _) = True
isWand _ = False

isTrap :: Object -> Bool
isTrap (Trap _ _) = True
isTrap _ = False

isLauncher :: Object -> Bool
isLauncher (Launcher _ _ _ _) = True
isLauncher _ = False

isWeapon :: Object -> Bool
isWeapon (Weapon _ _ _) = True
isWeapon _ = False

isMissile :: Object -> Bool
isMissile (Missile _ _ _ _) = True
isMissile _ = False

isArmor :: Object -> Bool
isArmor (Armor _ _ _ _) = True
isArmor _ = False

isJewelry :: Object -> Bool
isJewelry (Jewelry _ _ _ _ _) = True
isJewelry _ = False

isExistingBinding :: Monster -> Char -> Bool
isExistingBinding mon c = elem c $ join $ map objectKeys $ parts mon

isExistingBindingFirst :: World -> Char -> Bool
isExistingBindingFirst = isExistingBinding . getFirst
