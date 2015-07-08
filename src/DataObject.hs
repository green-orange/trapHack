module DataObject where

import DataDef

import System.Random (StdGen)

type InvGen = StdGen -> (Inv, StdGen)
	
objdmg :: Object -> StdDmg
objdmg obj w = (fmap (+ enchantment obj) n, g) where
	(n, g) = objdmg' obj w

count :: Object -> Int
count obj = count' obj + enchantment obj

ac :: Object -> Int
ac obj = ac' obj + enchantment obj

weight :: Object -> Int
weight (Potion {})  = 20
weight (Wand {})    = 7
weight (Scroll {})  = 5
weight (Trap {})    = 50
weight (Missile {}) = 2
weight (Jewelry {}) = 10
weight obj = weight' obj

instance Eq Object where
	(Potion t _ _) == (Potion t' _ _) = t == t'
	(Trap t _ _) == (Trap t' _ _) = t == t'
	(Missile t _ _ _ _) == (Missile t' _ _ _ _) = t == t'
	(Scroll t _ _) == (Scroll t' _ _) = t == t'
	_ == _ = False

isStackable :: Object -> Bool
isStackable obj = obj == obj

