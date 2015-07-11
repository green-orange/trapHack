module Read where

import DataDef
import Data
import Stuff
import Colors
import Init
import Texts

import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

takeDigits, dropDigits :: String -> String
takeDigits = takeWhile (`elem` ['0'..'9'])
dropDigits = dropWhile (`elem` ['0'..'9'])


myReadList :: Read a => Char -> String -> [a]
myReadList c str = map read $ separate c str

myReadByCoords :: Read a => String -> ((Int, Int), a)
myReadByCoords str = ((read x, read y), read obj) where
	cont = init $ tail $ tail str
	x = takeDigits cont
	rest = tail $ dropDigits cont
	y = takeDigits rest
	obj = tail $ tail $ dropDigits rest

myReadListCoords :: Read a => Char -> String -> [((Int, Int), a)]
myReadListCoords c str = map myReadByCoords $ separate c str

myReadInvElem :: Read a => String -> (Char, (a, Int))
myReadInvElem str = (c, (read obj, read n)) where
	c = str !! 2
	cont = drop 6 $ init $ init str
	n = reverse $ takeDigits $ reverse cont
	obj = reverse $ tail $ dropDigits $ reverse cont

myReadInv :: Read a => Char -> String -> [(Char, (a, Int))]
myReadInv _ "" = []
myReadInv c str = map myReadInvElem $ separate c str

myReadItem :: Read a => String -> (Int, Int, a, Int)
myReadItem str = (read x, read y, read obj, read z) where
	cont = tail $ init str
	x = takeDigits cont
	rest = tail $ dropDigits cont
	y = takeDigits rest
	rest' = tail $ dropDigits rest
	z = reverse $ takeDigits $ reverse rest'
	obj = reverse $ tail $ dropDigits $ reverse rest'

myReadItems :: Read a => Char -> String -> [(Int, Int, a, Int)]
myReadItems _ "" = []
myReadItems c str = map myReadItem $ separate c str

instance Read Object where
	readsPrec _ str = [(case objClass of
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
		_ -> error $ "parse error: object: " ++ objClass, "")]
		where
		parse = separate objSep str
		objClass : idStr : rest = parse
		enchStr : rest' = rest
		bindStr : _ = rest'
		id', bind', ench' :: Int
		id' = read idStr
		bind' = read bindStr
		ench' = read enchStr

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

instance Read World where
	readsPrec _ str = [(World {
		units' = read units'',
		message = [("Welcome again!", dEFAULT)],
		items = myReadItems listSepW items',
		action = ' ',
		stdgen = read stdgen',
		wave = read wave',
		chars = S.empty,
		worldmap = A.listArray ((0,0), (maxX, maxY)) 
			$ myReadList listSepW worldmap',
		dirs = rectdirs (0, 0, maxX, maxY),
		stepsBeforeWave = read stepsBeforeWave',
		prevAction = ' ',
		shift = 0,
		slot = toEnum 0,
		xInfo = 0,
		yInfo = 0
	}, "")] where
		parse = separate worldSep str
		[units'', items', stdgen', wave', worldmap', stepsBeforeWave'] = parse
