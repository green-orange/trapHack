module Utils.Items where

import Data.World
import Data.Define

import Control.Applicative ((<$>))

-- | has given object an appropriate constructor?

isPotion, isScroll, isWand, isTrap, isLauncher, isWeapon, isMissile, 
	isArmor, isJewelry, isFood, isResource, isTool :: Object -> Bool
isPotion (Potion {}) = True
isPotion _ = False

isScroll (Scroll {}) = True
isScroll _ = False

isWand (Wand {}) = True
isWand _ = False

isTrap (Trap {}) = True
isTrap _ = False

isLauncher (Launcher {}) = True
isLauncher _ = False

isWeapon (Weapon {}) = True
isWeapon _ = False

isMissile (Missile {}) = True
isMissile _ = False

isArmor (Armor {}) = True
isArmor _ = False

isJewelry (Jewelry {}) = True
isJewelry _ = False

isFood (Food {}) = True
isFood _ = False

isResource (Resource {}) = True
isResource _ = False

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
weight (Trap {})     = 2
weight (Missile {})  = 2
weight (Jewelry {})  = 5
weight (Resource {}) = 3
weight obj = weight' obj

-- | return 'Resource' with given type
itemFromRes :: ResourceType -> Object
itemFromRes rt = Resource {title = show rt, restype = rt}
