module Read where

import DataDef
import Data
import Stuff
import Colors
import Init

import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map as M

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
		"Jewelry"
			| bind' == hEAD -> (uNIQUEaMULETS !! id') ench'
			| bind' == aRM -> (uNIQUErINGS !! id') ench'
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
		parts = read parts',
		name = monNames !! read id',
		stddmg = read stddmg',
		inv = read inv',
		slowness = read slowness',
		time = read time',
		res = read res',
		intr = read intr',
		temp = read temp',
		idM = read id'
	}, "")] where
		parse = separate monSep str
		[ai', parts', stddmg', inv', slowness', time', res', intr', temp', 
			id'] = parse

instance Read Units where
	readsPrec _ str = [(Units {
		xF = xNew,
		yF = yNew,
		getFirst' = listNew M.! (xNew, yNew),
		list = listNew
	}, "")] where
		parse = separate unitsSep str
		[xF', yF', list'] = parse
		xNew = read xF'
		yNew = read yF'
		listNew = read list'
		
 
instance Read World where
	readsPrec _ str = [(World {
		units' = read units'',
		message = [("Welcome again!", dEFAULT)],
		items = read items',
		action = ' ',
		stdgen = read stdgen',
		wave = read wave',
		chars = S.empty,
		worldmap = A.listArray ((0,0), (maxX, maxY)) $ read worldmap',
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
