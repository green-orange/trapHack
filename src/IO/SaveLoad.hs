module IO.SaveLoad where

import Data.Define
import Items.Stuff
import IO.Texts
import IO.Colors

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Set (Set)
import Data.Functor ((<$>))
import System.Random (StdGen)
import Control.Arrow (first)

data SaveObject =
	-- | potion is determined by id
	Potion_ {_idO :: Int} | 
	-- | wand is determined by id and its charge
	Wand_ {_charge :: Int, _idO :: Int} |
	-- | scroll is determined by id
	Scroll_ {_idO :: Int} | 
	-- | trap is determined by id
	Trap_ {_idO :: Int} |
	-- | missile is determined by id and enchantment
	Missile_ {_enchantment :: Int, _idO :: Int} |
	-- | launcher is determined by id and enchantment
	Launcher_ {_enchantment :: Int, _idO :: Int} |
	-- | weapon is determined by id and enchantment
	Weapon_ {_enchantment :: Int, _idO :: Int} |
	-- | armor is determined by kind of body part to bind, id and enchantment
	Armor_ {_bind :: PartKind, _enchantment :: Int, _idO :: Int} |
	-- | jewelry is determined by kind of body part to bind, id and enchantment
	Jewelry_ {_bind :: PartKind, _enchantment :: Int, _idO :: Int} |
	-- | food saves all info but berry effect 
	Food_ {
		_title :: String,
		_nutrition :: Int,
		_weight' :: Int,
		_rotRate :: Int,
		_rotTime :: Int,
		_isBerry :: Bool,
		_idO :: Int
	} |
	-- | resource save all info
	Resource_ {_title :: String, _restype :: ResourceType} |
	-- | tools is determined by id and charge
	Tool_ {_charge :: Int, _idO :: Int}
	deriving (Show, Read)

saveObject :: Object -> SaveObject
saveObject (Potion _ _ idO_) = Potion_ {_idO = idO_}
saveObject (Wand _ _ _ charge_ idO_) = Wand_ {_charge = charge_, _idO = idO_}
saveObject (Scroll _ _ idO_) = Scroll_ {_idO = idO_}
saveObject (Trap _ _ idO_) = Trap_ {_idO = idO_}
saveObject (Missile _ _ _ enchantment_ idO_) =
	Missile_ {_enchantment = enchantment_, _idO = idO_}
saveObject (Launcher _ _ _ _ enchantment_ idO_) = 
	Launcher_ {_enchantment = enchantment_, _idO = idO_}
saveObject (Weapon _ _ _ enchantment_ idO_) = 
	Weapon_ {_enchantment = enchantment_, _idO = idO_}
saveObject (Armor _ _ _ bind_ enchantment_ idO_) =
	Armor_ {_bind = bind_, _enchantment = enchantment_, _idO = idO_}
saveObject (Jewelry _ _ _ bind_ enchantment_ idO_) =
	Jewelry_ {_bind = bind_, _enchantment = enchantment_, _idO = idO_}
saveObject (Food _ title_ nutrition_ weight'_ rotRate_ rotTime_	isBerry_ idO_) = Food_ {
	_title = title_,
	_nutrition = nutrition_,
	_weight' = weight'_,
	_rotRate = rotRate_,
	_rotTime = rotTime_,
	_isBerry = isBerry_,
	_idO = idO_
}
saveObject (Resource title_ restype_) = Resource_ {_title = title_, _restype = restype_}
saveObject (Tool _ _ _ charge_ idO_) = Tool_ {_charge = charge_, _idO = idO_}

loadObject :: SaveObject -> Object
loadObject (Potion_ id_) = potions !! id_
loadObject (Wand_ ch id_) = uniqueWands !! id_ $ ch
loadObject (Scroll_ id_) = scrolls !! id_
loadObject (Trap_ id_) = traps !! id_
loadObject (Missile_ ench id_) = (missiles !! id_) {enchantment = ench}
loadObject (Launcher_ ench id_) = (launchers !! id_) {enchantment = ench}
loadObject (Weapon_ ench id_) = (uniqueWeapons !! id_) {enchantment = ench}
loadObject (Armor_ bind_ ench id_) = (armorByType !! fromEnum bind_ !! id_) {enchantment = ench}
loadObject (Jewelry_ bind_ ench id_) = jewelryByType !! fromEnum bind_ !! id_ $ ench
loadObject (Food_ title_ nutrition_ weight_ rotRate_ rotTime_ isBerry_ id_) = Food {
	effect = if isBerry_ then effect $ berries !! id_ else id,
	title = title_,
	nutrition = nutrition_,
	weight' = weight_,
	rotRate = rotRate_,
	rotTime = rotTime_,
	isBerry = isBerry_,
	idO = id_
}
loadObject (Resource_ title_ restype_) = Resource {title = title_, restype = restype_}
loadObject (Tool_ ch id_) = (tools !! id_) {charge = ch}

type SaveInv = M.Map Char (SaveObject, Int)

saveInv :: Inv -> SaveInv
saveInv = fmap $ first saveObject

loadInv :: SaveInv -> Inv
loadInv = fmap $ first loadObject

data SaveMonster = SaveMonster {
	_ai :: AI,
	_parts :: [Part],
	_name :: String,
	_stddmg :: ((Int, Int), Float),
	_inv :: SaveInv,
	_slowness :: Int,
	_time :: Int,
	_res :: [Int],
	_intr :: [Int],
	_temp :: [Maybe Int],
	_idM :: Int,
	_xp :: Int
} deriving (Show, Read)

saveMonster :: Monster -> SaveMonster
saveMonster (Monster ai_ parts_ name_ stddmg_ inv_ slowness_ time_ res_ intr_ temp_ idM_ xp_) = 
	SaveMonster ai_ parts_ name_ stddmg_ (saveInv inv_) slowness_ time_ res_ intr_ temp_ idM_ xp_

loadMonster :: SaveMonster -> Monster
loadMonster (SaveMonster ai_ parts_ name_ stddmg_ inv_ slowness_ time_ res_ intr_ temp_ idM_ xp_) = 
	Monster ai_ parts_ name_ stddmg_ (loadInv inv_) slowness_ time_ res_ intr_ temp_ idM_ xp_

data SaveUnits = SaveUnits {
	_xF :: Int,
	_yF :: Int,
	_getFirst' :: SaveMonster,
	_list :: M.Map (Int, Int) SaveMonster
} deriving (Show, Read)

saveUnits :: Units -> SaveUnits
saveUnits (Units x_ y_ getFirst_ list_) = SaveUnits x_ y_ (saveMonster getFirst_) (saveMonster <$> list_)

loadUnits :: SaveUnits -> Units
loadUnits (SaveUnits x_ y_ getFirst_ list_) = Units x_ y_ (loadMonster getFirst_) (loadMonster <$> list_)

data SaveWorld = SaveWorld {
	  _units' :: SaveUnits
	, _items :: [(Int, Int, SaveObject, Int)]
	, _stdgen :: StdGen
	, _wave :: Int
	, _chars :: Set Char
	, _worldmap :: A.Array (Int, Int) Cell
	, _prevAction :: Char
	, _shift :: Int
	, _slot :: Slot
	, _xInfo :: Int
	, _yInfo :: Int
	, _numToSplit :: Int 
	, _colorHeight :: ColorHeight
	, _symbolHeight :: SymbolHeight
	, _mapType :: MapGenType 
} deriving (Show, Read)

saveWorld :: World -> SaveWorld
saveWorld (World units_ _ items_ _ stdgen_ wave_ chars_ worldmap_ prevAction_ shift_
	slot_ xInfo_ yInfo_ numToSplit_ colorHeight_ symbolHeight_ mapType_) =
	SaveWorld savedUnits savedItems stdgen_ wave_ chars_ worldmap_ prevAction_ shift_
	slot_ xInfo_ yInfo_ numToSplit_ colorHeight_ symbolHeight_ mapType_ where
		savedUnits = saveUnits units_
		savedItems = (\(x, y, obj, n) -> (x, y, saveObject obj, n)) <$> items_

loadWorld :: SaveWorld -> World
loadWorld (SaveWorld units_  items_  stdgen_ wave_ chars_ worldmap_ prevAction_ shift_
	slot_ xInfo_ yInfo_ numToSplit_ colorHeight_ symbolHeight_ mapType_) =
	World loadedUnits loadedMessage loadedItems Move stdgen_ wave_ chars_ worldmap_ prevAction_ shift_
	slot_ xInfo_ yInfo_ numToSplit_ colorHeight_ symbolHeight_ mapType_ where
		loadedUnits = loadUnits units_
		loadedItems = (\(x, y, obj, n) -> (x, y, loadObject obj, n)) <$> items_
		loadedMessage = [(msgAfterSave, defaultc)] 
