module Utils.AI where

import Data.Const
import Data.World
import Data.Monster
import Data.Define
import Utils.Changes
import Utils.Items
import Items.Items
import IO.Texts

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)
import Data.Maybe
import System.Random (randomR)
import Data.Functor ((<$>))

-- | return some not binded object with given condition
getterByCond :: (Object -> Bool) -> World -> Maybe Char
getterByCond cond world = safeMinFst $ M.filterWithKey fun $ inv $ getFirst world where
	fun c (o, _) = not (isExistingBindingFirst world c) && cond o

-- | is this monster need to heal at least one of its bodies?
needToBeHealedM :: Monster -> Bool
needToBeHealedM mon =
	any (\x -> kind x == Body && needToBeHealed x) $ parts mon

-- | is this part need to be healed?
needToBeHealed :: Part -> Bool
needToBeHealed part = 2 * hp part < maxhp part

-- | is this monster have source of healing?
canBeHealed :: Monster -> Bool
canBeHealed mon = M.foldr (||) False $ (isHealing . fst) <$> inv mon

-- | is this object healing?
isHealing :: Object -> Bool
isHealing obj = title obj == "potion of healing"

-- | return index of object to heal
healingAI :: World -> Char
healingAI = fromJust . getterByCond isHealing

-- | can this monster zap smth to attack?
canZapToAttack :: Monster -> Bool
canZapToAttack mon = M.foldr (||) False $ (isAttackWand . fst) <$> inv mon

-- | can this monster fire smth?
canFire :: Monster -> Bool
canFire mon = any (isValidMissile mon) alphabet

-- | can this monster eat now?
canEat :: Monster -> Bool
canEat mon = M.foldr (||) False $ (isFood . fst) <$> inv mon

-- | need this monster eat?
needEat :: Monster -> Bool
needEat mon = temp mon !! fromEnum Nutrition <= Just 5

-- | return index of some food in monster inventory
foodAI :: World -> Char
foodAI = fromJust . getterByCond isFood

-- | can this monster use given missile with current launcher?
isValidMissile :: Monster -> Char -> Bool
isValidMissile mon c = case objs of
	Nothing -> False
	Just (obj, _) ->
		let intended = filter (\w -> launcher obj == category w) launchers
		in isMissile obj && not (null intended)
	where
		objs = M.lookup c $ inv mon
		launchers = filter isLauncher $ fst <$> catMaybes
			(getItem WeaponSlot mon <$> parts mon)

-- | have this monster any launcher?
haveLauncher :: Monster -> Bool
haveLauncher mon = M.foldr (||) False $ (isLauncher . fst) <$> inv mon

-- | is this object an attack wand
isAttackWand :: Object -> Bool
isAttackWand obj = isWand obj && charge obj > 0 && 
	elem (title obj) ["wand of striking", "wand of radiation",
	"wand of poison", "wand of slowing", "wand of stun"]

-- | return index of an attack wand
zapAI :: World -> Char
zapAI = fromJust . getterByCond isAttackWand

-- | return index of stack of valid missiles
missileAI :: World -> Char
missileAI world = fromMaybe (putWE "missileAI") 
	$ listToMaybe $ filter (isValidMissile mon) alphabet where
	mon = getFirst world

-- | return key of the minimum element if the map if it exist
safeMinFst :: (Ord k) => M.Map k a -> Maybe k
safeMinFst m = 
	if M.null m
	then Nothing
	else Just $ fst $ M.findMin m

weaponAI, launcherAI :: World -> Maybe Char
-- | return index of some weapon
weaponAI = getterByCond isWeapon
-- | return index of some launcher
launcherAI = getterByCond isLauncher

-- | is this object an armor and is it bind to this kind of body part?
isArmorByKind :: PartKind -> Object -> Bool
isArmorByKind knd obj = isArmor obj && bind obj == knd

-- | return index of some armor with given condition
getArmorByKind :: PartKind -> World -> Maybe Char
getArmorByKind = getterByCond . isArmorByKind

-- | are these cells on the one vertical, horizontal or diagonal line
-- and with distance at most 'd'
isOnLine :: Int -> Int -> Int -> Int -> Int -> Bool
isOnLine d x1 y1 x2 y2 = abs (x1 - x2) <= d && abs (y1 - y2) <= d &&
	(x1 == x2 || y1 == y2 || x1 - y1 == x2 - y1 || x1 + y1 == x2 + y2)

-- | dir to char: 'dir' ('undir' c) === c if c is 1..9 or vi key
undir :: Int -> Int -> Char
undir   0  (-1) = 'k'
undir   0    1  = 'j'
undir (-1)   0  = 'h'
undir   1    0  = 'l'
undir (-1) (-1) = 'y'
undir   1  (-1) = 'u'
undir (-1)   1  = 'b'
undir   1    1  = 'n'
undir   0    0  = '.'
undir   _    _  = error "wrong direction (in function undir)"

-- | use item with given index in inventory if it's useful
usefulItem :: Object -> Char -> Maybe (World -> World)
usefulItem obj c
	| title obj == "potion of intellect" ||
		title obj == "potion of mutation" =
		Just $ fst . quaffFirst c
	| title obj == "wand of speed" && charge obj > 0 =
		Just $ zapMon '.' c
	| otherwise = Nothing

-- | use any useful item if exist
useSomeItem :: [Object] -> String -> Maybe (World -> World)
useSomeItem [] _ = Nothing
useSomeItem _ [] = Nothing
useSomeItem (obj:objs) (c:cs) = case usefulItem obj c of
	Nothing -> useSomeItem objs cs
	f -> f

-- | add temporary effect to given direction with given bounds of the duration
addTempByCoords :: Temp -> (Int, Int) -> Int -> Int -> World -> World
addTempByCoords t durs dx dy w = w {units' = newMons, stdgen = g} where
	(dur, g) = randomR durs $ stdgen w
	xNew = xFirst w + dx
	yNew = yFirst w + dy
	maybeMon = M.lookup (xNew, yNew) $ units w
	newMons = case maybeMon of
		Nothing -> units' w
		Just mon -> insertU (xNew, yNew) (setMaxTemp t (Just dur) mon) 
			$ units' w

-- | return coords of current player and direction to the player
coordsFromWorld :: Int -> Int -> World -> (Int, Int, Int, Int)
coordsFromWorld xP yP w = 
	(xNow, yNow, signum $ xP - xNow, signum $ yP - yNow) where
		xNow = xFirst w
		yNow = yFirst w

-- | is some item lying on a cell with given coords?
isItem :: Int -> Int -> World -> Bool
isItem x y w = any (\(x', y', _, _) -> x == x' && y == y') (items w)

-- | is any item lying on the cell with current monster?
isItemHere :: World -> Bool		
isItemHere w = isItem (xFirst w) (yFirst w) w

-- | record with coordinates of begin, current and end points
data Path = Path {
	xBegin, yBegin, xMid, yMid, xEnd, yEnd :: Int
} deriving (Eq, Show)

-- | compare paths by sum of length from begin to mid and from mid to end
-- using l_5 metric; return LT if length are equal but pathes are different
instance Ord Path where
	compare p1 p2 = 
		if p1 == p2
		then EQ
		else case (compare `on` pathLen) p1 p2 of
			GT -> GT
			LT -> LT
			EQ -> LT
		where
		rho x y = (fromIntegral x ** 5 + fromIntegral y ** 5) ** 0.2
		pathLen :: Path -> Float
		pathLen p = rho (abs (xMid p - xBegin p)) (abs (yMid p - yBegin p)) +
			rho (abs (xMid p - xEnd p)) (abs (yMid p - yEnd p))

-- | run A* pathfinding algorithm with gicen safety function,
-- begin ane end of the path
runAStar :: (World -> Int -> Int -> Int -> Int -> Bool) ->
	(Int, Int) -> (Int, Int) -> World -> Maybe (Int, Int)
runAStar safetyFun begin end world = 
	if begin == end then Nothing
	else aStar safetyFun begin end S.empty (S.singleton Path {
		xBegin = fst begin,
		yBegin = snd begin,
		xEnd = fst end,
		yEnd = snd end,
		xMid = fst begin,
		yMid = snd begin
	}) world

-- | A* pathfinding algorithm
aStar :: (World -> Int -> Int -> Int -> Int -> Bool) 
	-> (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> S.Set Path 
	-> World -> Maybe (Int, Int)
aStar safetyFun begin end@(xEnd', yEnd') closed paths w
	| S.null paths = Nothing
	| (xBest, yBest) `S.member` closed = aStar safetyFun begin end closed
		(S.delete best paths) w
	| end `elem` nears = Just (xBest - xEnd', yBest - yEnd')
	| otherwise = aStar safetyFun begin end newClosed newPaths w where
		best = S.findMin paths
		(xBest, yBest) = (xMid best, yMid best)
		d = [-1, 0, 1]
		nears = filter (`S.notMember` newClosed)
			[(xBest + dx, yBest + dy) | dx <- d, dy <- d, 
			safetyFun w (xBest + dx) (yBest + dy) (-dx) (-dy),
			abs (xBest + dx - xEnd') <= xSight, 
			abs (yBest + dy - yEnd') <= ySight]
		getPath (x, y) = Path {
			xBegin = fst begin,
			yBegin = snd begin,
			xEnd = fst end,
			yEnd = snd end,
			xMid = x,
			yMid = y
		}
		newClosed = S.insert (xBest, yBest) closed
		add = S.fromList $ getPath <$> nears
		newPaths = S.delete best $ S.union paths add
