module IO.Read where

import Data.Define
import Data.Const
import Items.Stuff
import IO.Colors
import IO.Texts
import Init

import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Functor ((<$>))

takeDigits, dropDigits :: String -> String
takeDigits = takeWhile (`elem` ['0'..'9'])
dropDigits = dropWhile (`elem` ['0'..'9'])

separate :: Char -> String -> [String]
separate _ [] = [""]
separate c s = first : 
	case rest of
		[] -> []
		_:rs -> separate c rs
	where
		first = takeWhile (c /=) s
		rest = dropWhile (c /=) s

myReadList :: Read a => Char -> String -> [a]
myReadList c str = read <$> separate c str

myReadByCoords :: Read a => String -> ((Int, Int), a)
myReadByCoords str = ((read x, read y), read obj) where
	cont = init $ tail $ tail str
	x = takeDigits cont
	_:rest = dropDigits cont
	y = takeDigits rest
	_:_:obj = dropDigits rest

myReadListCoords :: Read a => Char -> String -> [((Int, Int), a)]
myReadListCoords c str = myReadByCoords <$> separate c str

myReadInvElem :: Read a => String -> (Char, (a, Int))
myReadInvElem str = (c, (read obj, read n)) where
	_:_:c:_:_:_:cont = init $ init str
	n = reverse $ takeDigits $ reverse cont
	obj = reverse $ tail $ dropDigits $ reverse cont

myReadInv :: Read a => Char -> String -> [(Char, (a, Int))]
myReadInv _ "" = []
myReadInv c str = myReadInvElem <$> separate c str

myReadItem :: Read a => String -> (Int, Int, a, Int)
myReadItem str = (read x, read y, read obj, read z) where
	_:cont = init str
	x = takeDigits cont
	_:rest = dropDigits cont
	y = takeDigits rest
	_:rest' = dropDigits rest
	z = reverse $ takeDigits $ reverse rest'
	obj = reverse $ tail $ dropDigits $ reverse rest'

myReadItems :: Read a => Char -> String -> [(Int, Int, a, Int)]
myReadItems _ "" = []
myReadItems c str = myReadItem <$> separate c str

instance Read Object where
	readsPrec _ str = [(case arg1 of
		"Potion" -> pOTIONS !! id'
		"Wand" -> (uNIQUEwANDS !! id') ench'
		"Scroll" -> sCROLLS !! id'
		"Trap" -> tRAPS !! id'
		"Missile" -> (mISSILES !! id') {enchantment = ench'}
		"Launcher" -> (lAUNCHERS !! id') {enchantment = ench'}
		"Weapon" -> (uNIQUEwEAPONS !! id') {enchantment = ench'}
		"Armor"
			| bind' == hEAD -> (uNIQUEhELMS !! id') {enchantment = ench'}
			| bind' == bODY -> (uNIQUEaRMOR !! id') {enchantment = ench'}
			| bind' == lEG  -> (uNIQUEbOOTS !! id') {enchantment = ench'}
			| bind' == aRM -> (uNIQUEgLOVES !! id') {enchantment = ench'}
			| otherwise -> error $ "parse error: part: " ++ show bind'
		"Jewelry"
			| bind' == hEAD -> (uNIQUEaMULETS !! id') ench'
			| bind' == aRM -> (uNIQUErINGS !! id') ench'
			| otherwise -> error $ "parse error: part: " ++ show bind'
		"Food" -> Food {title = read arg2, nutrition = read arg3, 
			weight' = read arg4, rotRate = read arg5, rotTime = read arg6}
		"Resource" -> Resource {title = arg2, restype = read arg2}
		"Tool" -> (tOOLS !! id') {charge = ench'}
		_ -> error $ "parse error: " ++ str, "")]
		where
		parse = separate objSep str
		arg1 : arg2 : rest = parse
		arg3 : rest' = rest
		arg4 : rest'' = rest'
		arg5 : arg6 : _ = rest''
		id', bind', ench' :: Int
		id' = read arg2
		ench' = read arg3
		bind' = read arg4

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
		getFirst' = fromMaybe (error $ msgWE "Read Units") 
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
		dirs = rectdirs (0, 0, maxX, maxY),
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
