module Data.Define where

import qualified Data.Map as M
import qualified Data.Array as A
import System.Random (StdGen)
import Data.Set (Set)

monNames :: [String]
monNames = ["You", "Homunculus", "Beetle", "Bat", "Hunter", "Accelerator", "Troll",
	"Worm", "Floating eye", "Red dragon", "White dragon", "Green dragon",
	"Spider", "Soldier", "Umber hulk", "Ivy", "Tail", "Garbage collector",
	"Golem", "Dummy", "Rock", "Forgotten beast", "Tree"]

listSepMon, listSepW, listSepUn :: Char
listSepMon = '&'
listSepW = '*'
listSepUn = '%'

myShowList :: Show a => Char -> [a] -> String
myShowList _ [] = ""
myShowList _ [x] = show x
myShowList c (x:xs) = show x ++ [c] ++ myShowList c xs

data Units = Units {
	xF :: Int,
	yF :: Int,
	getFirst' :: Monster,
	list :: M.Map (Int, Int) Monster
}

unitsSep :: Char
unitsSep = '^'
instance Show Units where
	show uns = show (xF uns) ++ [unitsSep] ++ show (yF uns) ++ [unitsSep]
		++ myShowList listSepUn (M.toList $ list uns)

data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: Int,
	idP :: Int,
	regRate :: Int,
	objectKeys :: String
} deriving (Show, Read)

data Monster = Monster {
	ai :: AI,
	parts :: [Part],
	name :: String,
	stddmg :: ((Int, Int), Float),
	inv :: Inv,
	slowness :: Int,
	time :: Int,
	res :: [Int],
	intr :: [Int],
	temp :: [Maybe Int],
	idM :: Int,
	xp :: Int
}

data Object =
	Potion {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen),
		idO :: Int
	} | 
	Wand {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen),
		range :: Int,
		charge :: Int,
		idO :: Int
	} |
	Scroll {
		title :: String,
		actw :: World -> World,
		idO :: Int
	} | 
	Trap {
		title :: String,
		num :: Terrain,
		idO :: Int
	} |
	Missile {
		title :: String,
		objdmg' :: StdDmg,
		launcher :: String,
		enchantment :: Int,
		idO :: Int
	} |
	Launcher {
		title :: String,
		count' :: Int,
		category :: String,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	Weapon {
		title :: String,
		objdmg' :: StdDmg,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	Armor {
		title :: String,
		ac' :: Int,
		bind :: Int,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	Jewelry {
		title :: String,
		enchantment :: Int,
		bind :: Int,
		effectOn :: Int -> Monster -> Monster,
		effectOff :: Int -> Monster -> Monster,
		idO :: Int
	} |
	Food {
		title :: String,
		nutrition :: Int,
		weight' :: Int,
		rotRate :: Int,
		rotTime :: Int
	} |
	Resource {
		title :: String,
		restype :: ResourceType
	}

data World = World {
	units' :: Units,
	message :: [(String, Int)],
	items :: [(Int, Int, Object, Int)],
	action :: Action,
	stdgen :: StdGen,
	wave :: Int,
	chars :: Set Char,
	worldmap :: A.Array (Int, Int) Cell,
	dirs :: (Int, Int, Int, Int) -> Maybe (Int, Int),
	stepsBeforeWave :: Int,
	prevAction :: Char,
	shift :: Int,
	slot :: Slot,
	xInfo :: Int,
	yInfo :: Int,
	numToSplit :: Int,
	showMode :: ShowMode
}

data Cell = Cell {
	terrain :: Terrain,
	height :: Int
}

data ShowMode = ColorHeight | ColorMonsters | NoHeight deriving (Eq)

data ResourceType = Tree deriving (Eq, Show, Read)

data Action = Move | Quaff | Read | Zap1 | Zap2 | Fire1 | Fire2 | Drop |
	DropMany | Bind | Eat | SetTrap | Inventory | Pick | Equip | Call | 
	Info | Save | Previous | AfterSpace | Split1 | Split2 | Craft | 
	Options deriving (Eq)

data AImod = AcceleratorAI | TrollAI | HealAI | ZapAttackAI | PickAI | 
	FireAI | WieldLauncherAI | WieldWeaponAI | BindArmorAI | 
	UseItemsAI | EatAI deriving (Show, Read, Enum)
data AIpure = NothingAI | StupidestAI | StupidAI | StupidParalysisAI | 
	StupidPoisonAI | StupidConfAI | RandomAI | WormAI | IvyAI | CollectorAI |
	GolemAI deriving (Show, Read)
data AIrepr = AIrepr {
	mods :: [AImod],
	attackIfCloseMode :: Maybe (Elem, Int),
	aipure :: AIpure
} deriving (Show, Read)

data AI = You | AI AIrepr deriving (Show, Read)
	
data Intr = Teleport deriving (Enum, Show, Bounded)

data Temp = Nutrition | Poison | Stun | Conf deriving (Enum, Show, Bounded)

data Terrain = Empty | BearTrap | FireTrap | PoisonTrap | MagicTrap 
	deriving (Eq)
data Slot = WeaponSlot | ArmorSlot | JewelrySlot deriving (Enum, Bounded, Eq)

sLOTS :: Int
sLOTS = fromEnum (maxBound :: Slot) - fromEnum (minBound :: Slot) + 1

data Elem = Fire | Poison' | Cold deriving (Enum, Bounded)

type Inv = M.Map Char (Object, Int)
type StdDmg = World -> (Maybe Int, StdGen)
type InvGen = StdGen -> (Inv, StdGen)
type Recipe = ([(ResourceType, Int)], Object)

instance Eq Object where
	(Potion t _ _) == (Potion t' _ _) = t == t'
	(Trap t _ _) == (Trap t' _ _) = t == t'
	(Missile t _ _ _ _) == (Missile t' _ _ _ _) = t == t'
	(Scroll t _ _) == (Scroll t' _ _) = t == t'
	(Resource _ r) == (Resource _ r') = r == r'
	_ == _ = False

{-Read & Show-}
instance Show Elem where
	show Fire = "Fire"
	show Poison' = "Poison"
	show Cold = "Cold"
	
instance Read Elem where
	readsPrec _ "Fire" = [(Fire, "")]
	readsPrec _ "Poison" = [(Poison', "")]
	readsPrec _ "Cold" = [(Cold, "")]
	readsPrec _ e = error $ "parse error: Elem " ++ e

cellSep :: Char
cellSep = '$'

instance Show Cell where
	show (Cell terr hei) = show terr ++ [cellSep] ++ show hei 

instance Show Terrain where
	show Empty = ""
	show BearTrap = "bear trap"
	show FireTrap = "fire trap"
	show PoisonTrap = "poison trap"
	show MagicTrap = "magic trap"

instance Read Terrain where
	readsPrec _ "" = [(Empty, "")]
	readsPrec _ "bear trap" = [(BearTrap, "")]
	readsPrec _ "fire trap" = [(FireTrap, "")]
	readsPrec _ "poison trap" = [(PoisonTrap, "")]
	readsPrec _ "magic trap" = [(MagicTrap, "")]
	readsPrec _ t = error $ "parse error: Terrain " ++ t

monSep :: Char
monSep = '\n'

instance Show Monster where 
	show mon = 
		show (ai mon) ++ [monSep] ++
		myShowList listSepMon (parts mon) ++ [monSep] ++
		show (stddmg mon) ++ [monSep] ++
		myShowList listSepMon (M.toList $ inv mon) ++ [monSep] ++
		show (slowness mon) ++ [monSep] ++
		show (time mon) ++ [monSep] ++
		myShowList listSepMon (res mon) ++ [monSep] ++
		myShowList listSepMon (intr mon) ++ [monSep] ++
		myShowList listSepMon (temp mon) ++ [monSep] ++
		show (idM mon) ++ [monSep] ++ show (xp mon)

objSep :: Char
objSep = '/'

instance Show Object where
	show o@(Potion {}) = "Potion" ++ [objSep] ++ show (idO o)
	show o@(Wand {}) = "Wand" ++ [objSep] ++ show (idO o) ++ [objSep] 
		++ show (charge o)
	show o@(Scroll {}) = "Scroll" ++ [objSep] ++ show (idO o)
	show o@(Trap {}) = "Trap" ++ [objSep] ++ show (idO o)
	show o@(Missile {}) = "Missile" ++ [objSep] ++ show (idO o) 
		++ [objSep] ++ show (enchantment o)
	show o@(Launcher {}) = "Launcher" ++ [objSep] ++ show (idO o) 
		++ [objSep] ++ show (enchantment o)
	show o@(Weapon {}) = "Weapon" ++ [objSep] ++ show (idO o) ++ [objSep] 
		++ show (enchantment o)
	show o@(Armor {}) = "Armor" ++ [objSep] ++ show (idO o) ++ [objSep] 
		++ show (enchantment o) ++ [objSep] ++ show (bind o) 
	show o@(Jewelry {}) = "Jewelry" ++ [objSep] ++ show (idO o) ++ [objSep] 
		++ show (enchantment o) ++ [objSep] ++ show (bind o)
	show o@(Food {}) = "Food" ++ [objSep] ++ show (title o) ++ [objSep] 
		++ show (nutrition o) ++ [objSep] ++ show (weight' o) ++ [objSep]
		++ show (rotRate o) ++ [objSep] ++ show (rotTime o) 
	show o@(Resource {}) = "Resource" ++ [objSep] ++ show (restype o)

worldSep :: Char
worldSep = '#'

instance Show World where
	show w = 
		show (units' w) ++ [worldSep] ++
		myShowList listSepW (items w) ++ [worldSep] ++
		show (stdgen w) ++ [worldSep] ++
		show (wave w) ++ [worldSep] ++
		myShowList listSepW (A.elems $ worldmap w) ++ [worldSep] ++
		show (stepsBeforeWave w)
