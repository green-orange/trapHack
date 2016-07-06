module IO.Read where

import Data.Define
import Data.Const
import Items.Stuff
import Monsters.Monsters
import IO.Colors
import IO.Texts

import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))

takeDigits, dropDigits :: String -> String
-- | take prefix of the string with digits only
takeDigits = takeWhile (`elem` ['0'..'9'])
-- | drop prefix of the string with digits only
dropDigits = dropWhile (`elem` ['0'..'9'])

-- | split string to a list of strings by given separator
separate :: Char -> String -> [String]
separate _ [] = [""]
separate c s = first : 
	case rest of
		[] -> []
		_:rs -> separate c rs
	where
		first = takeWhile (c /=) s
		rest = dropWhile (c /=) s

-- | method to read lists without parenthesis and commas;
-- inverse to myShowList
myReadList :: Read a => Char -> String -> [a]
myReadList c str = read <$> separate c str

-- | method to read pair (coord, smth)
myReadByCoords :: Read a => String -> ((Int, Int), a)
myReadByCoords str = ((read x, read y), read obj) where
	cont = init $ tail $ tail str
	x = takeDigits cont
	_:rest = dropDigits cont
	y = takeDigits rest
	_:_:obj = dropDigits rest

-- | method to read a list of pairs (coord, smth)
myReadListCoords :: Read a => Char -> String -> [((Int, Int), a)]
myReadListCoords c str = myReadByCoords <$> separate c str

-- | method to read element of the monster inventory
myReadInvElem :: Read a => String -> (Char, (a, Int))
myReadInvElem str = (c, (read obj, read n)) where
	_:_:c:_:_:_:cont = init $ init str
	n = reverse $ takeDigits $ reverse cont
	obj = reverse $ tail $ dropDigits $ reverse cont

-- | method to read the monster inventory
myReadInv :: Read a => Char -> String -> [(Char, (a, Int))]
myReadInv _ "" = []
myReadInv c str = myReadInvElem <$> separate c str

-- | method to read the item on a ground
myReadItem :: Read a => String -> (Int, Int, a, Int)
myReadItem str = (read x, read y, read obj, read z) where
	_:cont = init str
	x = takeDigits cont
	_:rest = dropDigits cont
	y = takeDigits rest
	_:rest' = dropDigits rest
	z = reverse $ takeDigits $ reverse rest'
	obj = reverse $ tail $ dropDigits $ reverse rest'

-- | method to read all items on a ground
myReadItems :: Read a => Char -> String -> [(Int, Int, a, Int)]
myReadItems _ "" = []
myReadItems c str = myReadItem <$> separate c str

instance Read Object where
	readsPrec _ str = [(case raw !! 0 of
		"Potion" -> potions !! id'
		"Wand" -> (uniqueWands !! id') ench'
		"Scroll" -> scrolls !! id'
		"Trap" -> traps !! id'
		"Missile" -> (missiles !! id') {enchantment = ench'}
		"Launcher" -> (launchers !! id') {enchantment = ench'}
		"Weapon" -> (uniqueWeapons !! id') {enchantment = ench'}
		"Armor" -> (armorByType !! bind' !! id') {enchantment = ench'}
		"Jewelry" -> jewelryByType !! bind' !! id' $ ench'
		"Food" -> Food {title = raw !! 1, nutrition = parsed !! 2 , 
			weight' = parsed !! 3, rotRate = parsed !! 4, rotTime = parsed !! 5,
			idO = parsed !! 6, effect = if isBerry' then effect 
			$ berries !! (parsed !! 6) else id, isBerry = isBerry'}
		"Resource" -> Resource {title = raw !! 1, restype = read $ raw !! 1}
		"Tool" -> (tools !! id') {charge = ench'}
		_ -> error $ "parse error: " ++ str, "")]
		where
		raw = separate objSep str
		parsed = map read raw
		isBerry' = raw !! 6 /= "-1"
		id', bind', ench' :: Int
		id' = parsed !! 1
		ench' = parsed !! 2
		bind' = parsed !! 3

instance Read Monster where
	readsPrec _ str = [(Monster {
		ai = read ai',
		parts = myReadList listSepMon parts',
		name = monNames !! read id',
		stddmg = read stddmg',
		inv = M.fromList $ myReadInv listSepMon inv',
		slowness = read slowness',
		time = read time',
		res = myReadList listSepMon res',
		intr = myReadList listSepMon intr',
		temp = myReadList listSepMon temp',
		idM = read id',
		xp = read xp'
	}, "")] where
		parse = separate monSep str
		[ai', parts', stddmg', inv', slowness', time', res', intr', temp', 
			id', xp'] = parse

instance Read Units where
	readsPrec _ str = [(Units {
		xF = xNew,
		yF = yNew,
		getFirst' = fromMaybe (putWE "Read Units") 
			$ M.lookup (xNew, yNew) listNew,
		list = listNew
	}, "")] where
		parse = separate unitsSep str
		[xF', yF', list'] = parse
		xNew = read xF'
		yNew = read yF'
		listNew = M.fromList $ myReadListCoords listSepUn list'

instance Read Cell where
	readsPrec _ str = [(Cell {
		terrain = read arg1,
		height = read arg2
	}, "")] where
		parse = separate cellSep str
		[arg1, arg2] = parse

instance Read World where
	readsPrec _ str = [(World {
		units' = read units'',
		message = [("", dEFAULT)],
		items = myReadItems listSepW items',
		action = Move,
		stdgen = read stdgen',
		wave = read wave',
		chars = S.empty,
		worldmap = A.listArray ((0,0), (maxX, maxY)) 
			$ myReadList listSepW worldmap',
		prevAction = ' ',
		shift = 0,
		slot = toEnum 0,
		xInfo = 0,
		yInfo = 0,
		numToSplit = 0,
		showMode = ColorMonsters,
		mapType = read mapType'
	}, "")] where
		parse = separate worldSep str
		[units'', items', stdgen', wave', worldmap', mapType'] = parse
