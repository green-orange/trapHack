module Utils.Items where

import Data.World
import Data.Define

import Control.Applicative ((<$>))

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

isFood :: Object -> Bool
isFood (Food {}) = True
isFood _ = False

isResource :: Object -> Bool
isResource (Resource {}) = True
isResource _ = False

isTool :: Object -> Bool
isTool (Tool {}) = True
isTool _ = False

-- | return damage from this item
objdmg :: Object -> StdDmg
objdmg obj w = ((+ enchantment obj) <$> n, g) where
	(n, g) = objdmg' obj w

-- | return count of missiles for given launcher
count :: Object -> Int
count obj = count' obj + enchantment obj

-- | return ac from this item
ac :: Object -> Int
ac obj = ac' obj + enchantment obj

-- | is this char some binding for this monster
isExistingBinding :: Monster -> Char -> Bool
isExistingBinding mon c = elem c $ concatMap objectKeys $ parts mon

-- | is this char some binding for current monster
isExistingBindingFirst :: World -> Char -> Bool
isExistingBindingFirst = isExistingBinding . getFirst

-- | return weight of the item
weight :: Object -> Int
weight (Potion {})   = 20
weight (Wand {})     = 7
weight (Scroll {})   = 5
weight (Trap {})     = 50
weight (Missile {})  = 2
weight (Jewelry {})  = 10
weight (Resource {}) = 3
weight obj = weight' obj

-- | return 'Resource' with given type
itemFromRes :: ResourceType -> Object
itemFromRes rt = Resource {title = show rt, restype = rt}
