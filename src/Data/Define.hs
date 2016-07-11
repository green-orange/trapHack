module Data.Define where

import qualified Data.Map as M
import qualified Data.Array as A
import System.Random (StdGen)
import Data.Set (Set)
import UI.HSCurses.Curses(ChType)

-- | a record type with all units in game and info about current step
data Units = Units {
	xF :: Int,
	yF :: Int,
	getFirst' :: Monster,
	list :: M.Map (Int, Int) Monster
}

-- | a record type with one body part include an item binds to it
data Part = Part {
	hp :: Int,
	maxhp :: Int,
	kind :: PartKind,
	idP :: Int,
	regRate :: Int,
	objectKeys :: M.Map Slot Char
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

-- | get an item by a slot, part and item; return Nothing if there is no item in given slot
getItem :: Slot -> Monster -> Part -> Maybe (Object, Int)
getItem slot_ mon part = M.lookup slot_ (objectKeys part) >>= flip M.lookup (inv mon)

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
		weight' :: Int,
		enchantment :: Int,
		idO :: Int
	} |
	-- | weapon can increment the damage produced by you
	Weapon {
		title :: String,
		objdmg' :: StdDmg,
		weight' :: Int,
		enchantment :: Int,
		idO :: Int
	} |
	-- | armor binds to differnt body parts and protect them
	-- from physical damage 
	Armor {
		title :: String,
		ac' :: Int,
		weight' :: Int,
		bind :: PartKind,
		enchantment :: Int,
		idO :: Int
	} |
	-- | jewelry binds to different body parts and gives
	-- you some magic abilities or intrinsics 
	Jewelry {
		title :: String,
		effectOn :: Int -> Monster -> Monster,
		effectOff :: Int -> Monster -> Monster,
		bind :: PartKind,
		enchantment :: Int,
		idO :: Int
	} |
	-- | food must be used to don't die from starvation 
	Food {
		effect :: Monster -> Monster,
		title :: String,
		nutrition :: Int,
		weight' :: Int,
		rotRate :: Int,
		rotTime :: Int,
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
	, colorHeight :: ColorHeight -- ^ color to show the heights on a worldmap
	, symbolHeight :: SymbolHeight -- ^ symbols to show heights on a worldmap
	, mapType :: MapGenType -- ^ type of the map generator (for scroll of safety) 
}

-- | record with one cell of the world
data Cell = Cell {
	terrain :: Terrain,
	height :: Int
} deriving (Show, Read)

-- | how to use colors to show height
data ColorHeight = Absolute | Relative | NoColor deriving (Show, Read)

-- | how to use characters to show height
data SymbolHeight = Numbers | SymbolHeight ChType deriving (Show, Read)

-- | default option for color height
defaultColorHeight :: ColorHeight
defaultColorHeight = Absolute

-- | symbol of a filled square that can be colored; it's really strange, yeah
filledSquare :: ChType
filledSquare = toEnum $ 97 + 2 ^ (22 :: Integer)

-- | default option for symbol height
defaultSymbolHeight :: SymbolHeight
defaultSymbolHeight = SymbolHeight filledSquare 

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
	Options | Use1 | Use2 deriving (Eq, Show, Read)

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
data Intr = Strength | Teleport deriving (Enum, Show, Bounded)

-- | temporary intrinsic decreased every step
data Temp = Nutrition | Poison | Stun | Conf deriving (Enum, Show, Bounded)

-- | type of the terrain (empty, water or some trap)
data Terrain = Empty | Water | BearTrap | FireTrap | PoisonTrap | MagicTrap 
	| Bonfire | MagicNatural deriving (Eq, Read, Show)

-- | item slot of the every part
data Slot = WeaponSlot | ArmorSlot | JewelrySlot deriving (Enum, Bounded, Eq, Ord, Show, Read)

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
data Elem = Fire | Poison' | Cold deriving (Enum, Bounded, Read, Show)

-- | monster inventory
type Inv = M.Map Char (Object, Int)
-- | standard damage output function (world used just as StdGen source)
type StdDmg = World -> (Maybe Int, StdGen)
-- | inventory generator to spawn a death drop
type InvGen = StdGen -> (Inv, StdGen)
-- | a craft recipe
type Recipe = ([(ResourceType, Int)], Object)

-- | type of a body part
data PartKind = Body | Head | Leg | Arm | Wing | Paw | Main deriving (Show, Read, Eq, Enum, Ord, Bounded)

-- | items are equal iff they can be put in a stack
instance Eq Object where
	(Potion t _ _) == (Potion t' _ _) = t == t'
	(Trap t _ _) == (Trap t' _ _) = t == t'
	(Missile t _ _ _ _) == (Missile t' _ _ _ _) = t == t'
	(Scroll t _ _) == (Scroll t' _ _) = t == t'
	(Resource _ r) == (Resource _ r') = r == r'
	_ == _ = False
