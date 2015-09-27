module Data.Define where

import qualified Data.Map as M
import qualified Data.Array as A
import System.Random (StdGen)
import Data.Set (Set)

-- | separators used to read/write lists in the save
listSepMon, listSepW, listSepUn :: Char
listSepMon = '&'
listSepW = '*'
listSepUn = '%'

-- | method to show lists without parenthesis and commas
myShowList :: Show a => Char -> [a] -> String
myShowList _ [] = ""
myShowList _ [x] = show x
myShowList c (x:xs) = show x ++ [c] ++ myShowList c xs

-- | a record type with all units in game and info about current step
data Units = Units {
	xF :: Int,
	yF :: Int,
	getFirst' :: Monster,
	list :: M.Map (Int, Int) Monster
}

-- | symbol to separate fields in 'Units' data
unitsSep :: Char
unitsSep = '^'
instance Show Units where
	show uns = show (xF uns) ++ [unitsSep] ++ show (yF uns) ++ [unitsSep]
		++ myShowList listSepUn (M.toList $ list uns)

-- | a record type with one body part include an item binds to it
data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: Int,
	idP :: Int,
	regRate :: Int,
	objectKeys :: String
} deriving (Show, Read)

-- | a record type with full info about one monster
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

-- | any item in the game
data Object =
	-- | potions can do something with a monster who quaff it
	Potion {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen),
		idO :: Int
	} | 
	-- | wands can do something with a monster which was zapped by the wand
	Wand {
		title :: String,
		act :: (Monster, StdGen) -> (Monster, StdGen),
		range :: Int,
		charge :: Int,
		idO :: Int
	} |
	-- | scroll can do something with all world, but often it acts
	-- only on small part of the world near to monster who read it
	Scroll {
		title :: String,
		actw :: World -> World,
		idO :: Int
	} | 
	-- | trap can be set on a cell and do something with monster
	-- who caught in it
	Trap {
		title :: String,
		num :: Terrain,
		idO :: Int
	} |
	-- | missile can be fired from a 'Launcher'
	Missile {
		title :: String,
		objdmg' :: StdDmg,
		launcher :: String,
		enchantment :: Int,
		idO :: Int
	} |
	-- | launcher can be used to fire 'Missile'
	Launcher {
		title :: String,
		count' :: Int,
		category :: String,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	-- | weapon can increment the damage produced by you
	Weapon {
		title :: String,
		objdmg' :: StdDmg,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	-- | armor binds to differnt body parts and protect them
	-- from physical damage 
	Armor {
		title :: String,
		ac' :: Int,
		bind :: Int,
		enchantment :: Int,
		weight' :: Int,
		idO :: Int
	} |
	-- | jewelry binds to different body parts and gives
	-- you some magic abilities or intrinsics 
	Jewelry {
		title :: String,
		enchantment :: Int,
		bind :: Int,
		effectOn :: Int -> Monster -> Monster,
		effectOff :: Int -> Monster -> Monster,
		idO :: Int
	} |
	-- | food must be used to don't die from starvation 
	Food {
		title :: String,
		nutrition :: Int,
		weight' :: Int,
		rotRate :: Int,
		rotTime :: Int,
		effect :: Monster -> Monster,
		isBerry :: Bool,
		idO :: Int
	} |
	-- | resources are items using by craft something from them
	Resource {
		title :: String,
		restype :: ResourceType
	} |
	-- | tools are all another items
	Tool {
		title :: String,
		tooltype :: ToolType,
		weight' :: Int,
		charge :: Int,
		idO :: Int
	}

-- | record with all world
data World = World {
	  units' :: Units -- ^ all 'Units' in the game
	, message :: [(String, Int)] -- ^ current message for player
	, items :: [(Int, Int, Object, Int)] -- ^ items on the ground
	, action :: Action -- ^ last monster action
	, stdgen :: StdGen -- ^ random number generator
	, wave :: Int -- ^ number of NEXT wave
	, chars :: Set Char -- ^ chars using for picking or multi-dropping
	, worldmap :: A.Array (Int, Int) Cell -- ^ world map with heights and terrains
	, prevAction :: Char -- ^ previous symbol pressed by the player
	, shift :: Int -- ^ vertical shift in equip menu
	, slot :: Slot -- ^ horizontal shift in equip menu
	, xInfo :: Int -- ^ current x coordinate when you show info about smth
	, yInfo :: Int -- ^ current y coordinate when you show info about smth
	, numToSplit :: Int -- ^ number using to Split command
	, showMode :: ShowMode -- ^ mode to show the worldmap
	, mapType :: MapGenType -- ^ type of the map generator (for scroll of safety) 
}

-- | record with one cell of the world
data Cell = Cell {
	terrain :: Terrain,
	height :: Int
}

-- | mode to show the worldmap
data ShowMode = ColorHeight | ColorHeightAbs | ColorMonsters | NoHeight
	deriving (Eq)

-- | type of the resource
data ResourceType = Tree | Stone | MetalScrap deriving (Eq)

instance Show ResourceType where
	show Tree = "tree"
	show Stone = "stone"
	show MetalScrap = "metal scrap"

instance Read ResourceType where
	readsPrec _ "tree" = [(Tree, "")]
	readsPrec _ "stone" = [(Stone, "")]
	readsPrec _ "metal scrap" = [(MetalScrap, "")]
	readsPrec _ r = error $ "Parse error: ResourceType" ++ r

-- | type of the tool
data ToolType = PickAxe deriving (Eq)

-- | current action in the game
data Action = Move | Quaff | Read | Zap1 | Zap2 | Fire1 | Fire2 | Drop |
	DropMany | Bind | Eat | SetTrap | Inventory | Pick | Equip | Call | 
	Info | Save | Previous | AfterSpace | Split1 | Split2 | Craft | 
	Options | Use1 | Use2 deriving (Eq)

-- | modificators of the AI
data AImod = AcceleratorAI | TrollAI | HealAI | ZapAttackAI | PickAI | 
	FireAI | WieldLauncherAI | WieldWeaponAI | BindArmorAI | 
	UseItemsAI | EatAI deriving (Show, Read, Enum)
-- | base AI types
data AIpure = NothingAI | StupidestAI | StupidAI | StupidParalysisAI | 
	StupidPoisonAI | StupidConfAI | RandomAI | WormAI | IvyAI | BushAI |
	CollectorAI | GolemAI | CleverSAI | CleverVSAI | CleverUAI 
	deriving (Show, Read)
-- | AI representation to read and show
data AIrepr = AIrepr {
	mods :: [AImod],
	attackIfCloseMode :: Maybe (Elem, Int),
	aipure :: AIpure
} deriving (Show, Read)

-- | monster AI or player
data AI = You | AI AIrepr deriving (Show, Read)

-- | abstract intrinsic
data Intr = Teleport deriving (Enum, Show, Bounded)

-- | temporary intrinsic decreased every step
data Temp = Nutrition | Poison | Stun | Conf deriving (Enum, Show, Bounded)

-- | type of the terrain (empty, water or some trap)
data Terrain = Empty | Water | BearTrap | FireTrap | PoisonTrap | MagicTrap 
	| Bonfire | MagicNatural deriving (Eq)

-- | item slot of the every part
data Slot = WeaponSlot | ArmorSlot | JewelrySlot deriving (Enum, Bounded, Eq)

-- | base type of height generator
data HeiGenType = Sin30 | Sin3 | Flat Int | Random | Mountains
	deriving (Show, Read)

-- | number of height averaging
type Avg = Int

-- | type of water using in the map
data Water = NoWater | Rivers Int | Swamp Int deriving (Show, Read)

-- | type of traps using in the map
data TrapMap = NoTraps | Bonfires Int | MagicMap Int deriving (Show, Read)

-- | full info about map generator
data MapGenType = MapGenType HeiGenType Avg Water TrapMap deriving (Show, Read)

-- | number of slots
sLOTS :: Int
sLOTS = fromEnum (maxBound :: Slot) - fromEnum (minBound :: Slot) + 1

-- | elements that can be used to special attacks
data Elem = Fire | Poison' | Cold deriving (Enum, Bounded)

-- | monster inventory
type Inv = M.Map Char (Object, Int)
-- | standard damage output function (world used just as StdGen source)
type StdDmg = World -> (Maybe Int, StdGen)
-- | inventory generator to spawn a death drop
type InvGen = StdGen -> (Inv, StdGen)
-- | a craft recipe
type Recipe = ([(ResourceType, Int)], Object)

-- | items are equal iff they can be put in a stack
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

-- | symbol to separate fields in 'Cell' data
cellSep :: Char
cellSep = '$'

instance Show Cell where
	show (Cell terr hei) = show terr ++ [cellSep] ++ show hei 

instance Show Terrain where
	show Empty = ""
	show Water = "water"
	show BearTrap = "bear trap"
	show FireTrap = "fire trap"
	show PoisonTrap = "poison trap"
	show MagicTrap = "magic trap"
	show Bonfire = "bonfire"
	show MagicNatural = "magic source"

instance Read Terrain where
	readsPrec _ "" = [(Empty, "")]
	readsPrec _ "bear trap" = [(BearTrap, "")]
	readsPrec _ "fire trap" = [(FireTrap, "")]
	readsPrec _ "poison trap" = [(PoisonTrap, "")]
	readsPrec _ "magic trap" = [(MagicTrap, "")]
	readsPrec _ "water" = [(Water, "")]
	readsPrec _ "bonfire" = [(Bonfire, "")]
	readsPrec _ "magic source" = [(MagicNatural, "")]
	readsPrec _ t = error $ "parse error: Terrain " ++ t

-- | symbol to separate fields in 'Monster' data
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

-- | symbol to separate fields in 'Object' data
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
		++ show (rotRate o) ++ [objSep] ++ show (rotTime o) ++ [objSep] 
		++ show (idO o)
	show o@(Resource {}) = "Resource" ++ [objSep] ++ show (restype o)
	show o@(Tool {}) = "Tool" ++ [objSep] ++ show (idO o) ++ [objSep] 
		++ show (charge o)

-- | symbol to separate fields in 'World' data
worldSep :: Char
worldSep = '#'

instance Show World where
	show w = 
		show (units' w) ++ [worldSep] ++
		myShowList listSepW (items w) ++ [worldSep] ++
		show (stdgen w) ++ [worldSep] ++
		show (wave w) ++ [worldSep] ++
		myShowList listSepW (A.elems $ worldmap w) ++ [worldSep] ++
		show (mapType w)
