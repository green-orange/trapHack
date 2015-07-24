module MapGen where

import Data.Define
import Data.Const
import Data.World
import Utils.Random

import System.Random
import qualified Data.Array as A
import Data.Functor ((<$>))
import Control.Arrow (first)

instance Num b => Num (a -> b) where
	(f + g) x = f x + g x
	(f - g) x = f x - g x
	(f * g) x = f x * g x
	fromInteger = const . fromInteger
	(abs f) x = abs $ f x
	(signum f) x = signum $ f x

runHei :: HeiGenType -> HeiGen
runHei Sin30 = getSinHei (-1.0) 2.0 30 40
runHei Sin3 = getSinHei 0.01 0.99 5 10
runHei Random = getRandomHeis
runHei Mountains = getMountains 0.1
runHei (Flat n) = getFlatMap n

runWater :: Water -> HeiGen -> MapGen
runWater NoWater = mapGenFromHeightGen
runWater (Rivers n) = addRiversToGen n
runWater (Swamp n) = addSwampsToGen n

runMap :: MapGenType -> MapGen
runMap (MapGenType heigen avg water) = runWater water $ foldr (.) 
	(limit *. runHei heigen) $ replicate avg $ first averaging

pureMapGen :: HeiGenType -> MapGenType
pureMapGen heigen = MapGenType heigen 0 NoWater

getSinFunc :: Float -> Float -> StdGen -> (Int -> Int -> Float, StdGen)
getSinFunc maxA maxB g = (sinf, g3) where
	(a, g1) = randomR (0.0, maxA) g
	(b, g2) = randomR (0.0, maxB) g1
	(c, g3) = randomR (0.0, 2 * pi) g2
	sinf x y = sin $ a * fromIntegral x + b * fromIntegral y + c

addSinFunc :: Float -> Float -> (Int -> Int -> Float, StdGen) 
	-> (Int -> Int -> Float, StdGen)
addSinFunc maxA maxB (f, gen) = (f + f', gen') where
	(f', gen') = getSinFunc maxA maxB gen

getSinHei :: Float -> Float -> Int -> Int -> HeiGen
getSinHei left right cntSin mult gen = (rez, newGen) where
	cntSinF :: Float
	cntSinF = fromIntegral cntSin
	maxX' = fromIntegral mult * pi / fromIntegral maxX
	maxY' = fromIntegral mult * pi / fromIntegral maxY
	(sinf, newGen) = (foldr (.) id $ replicate cntSin $ addSinFunc maxX' maxY')
		(const $ const 0, gen)
	rez = A.array ((0, 0), (maxX, maxY)) [((x, y), heighFromCoords 
		(fromIntegral x) (fromIntegral y)) 
		| x <- [0 .. maxX], y <- [0 .. maxY]]
	normalize (l, r) (l', r') x = l' + (x - l) * (r' - l') / (r - l)
	heighFromCoords x y = uniformFromList (max 0 $ min 0.99 
			$ normalize (-cntSinF, cntSinF) (left, right) $ sinf x y) [0..9]

type MapGen = StdGen -> (A.Array (Int, Int) Cell, StdGen)
type HeiGen = StdGen -> (A.Array (Int, Int) Int, StdGen)

infixr 9 *.
(*.) :: (a -> c) -> (b -> (a, b)) -> b -> (c, b)
(f *. g) x = (f rez, x') where
	(rez, x') = g x

limit :: A.Array (Int, Int) Int -> A.Array (Int, Int) Int
limit = fmap $ max 0 . min 9

averaging :: A.Array (Int, Int) Int -> A.Array (Int, Int) Int
averaging arr = A.array ((0, 0), (maxX, maxY))
	[((x, y), avg x y) | x <- [0..maxX], y <- [0..maxY]] where
	d = [-1..1]
	avg x y = (2 * (arr A.! (x, y)) + sum ((arr A.!) <$> nears)) 
		`div` (2 + length nears) where
		nears = [(x + dx, y + dy) | dx <- d, dy <- d,
			x + dx >= 0, y + dy >= 0, x + dx <= maxX, y + dy <= maxY]

mapFromHeights :: A.Array (Int, Int) Int -> A.Array (Int, Int) Cell
mapFromHeights = fmap (\h -> Cell {terrain = Empty, height = h})

mapGenFromHeightGen :: HeiGen -> MapGen
mapGenFromHeightGen hgen gen = (mapFromHeights heis, gen')
	where (heis, gen') = hgen gen

getFlatMap :: Int -> HeiGen
getFlatMap n g = (A.listArray ((0, 0), (maxX, maxY)) 
	[n, n..], g)

getRandomHeis :: HeiGen
getRandomHeis g = (A.listArray ((0, 0), (maxX, maxY)) 
	$ randomRs (0, 9) g', g'') where
	(g', g'') = split g
	
getMountains :: Float -> HeiGen
getMountains density gen = (A.array ((0, 0), (maxX, maxY))
	[((x, y), sumLand x y) | x <- [0..maxX], y <- [0..maxY]], g') where
	(g, g') = split gen
	(gx, gy)= split g
	xs = randomRs (0, maxX) gx
	ys = randomRs (0, maxY) gy
	cnt = round $ (* density) $ fromIntegral $ (1 + maxX) * (1 + maxY)
	mnts = take cnt $ zipWith getMnt xs ys
	vlls = take cnt $ drop cnt $ zipWith getVll xs ys 
	getMnt, getVll :: Int -> Int -> Int -> Int -> Float
	getMnt xMnt yMnt x y = (* 2) $ exp $ negate $ sqrt 
		$ fromIntegral $ (xMnt - x) ^ (2 :: Int) + (yMnt - y) ^ (2 :: Int)
	getVll xMnt yMnt x y = (* 2) $ (0.004 -) $ exp $ negate $ sqrt 
		$ fromIntegral $ (xMnt - x) ^ (2 :: Int) + (yMnt - y) ^ (2 :: Int)
	sumMnts x y = floor $ sum $ (\f -> f x y) <$> mnts
	sumVlls x y = floor $ sum $ (\f -> f x y) <$> vlls
	sumLand = sumMnts + sumVlls

addRiver :: Int -> Int -> (A.Array (Int, Int) Cell, StdGen)
	-> (A.Array (Int, Int) Cell, StdGen)
addRiver x y (wmap, g) =
	if null nears
	then (newWMap, g')
	else uncurry addRiver (uniformFromList q nears) (newWMap, g')
	where
	newWMap = wmap A.// [((x, y), Cell {terrain = Water, 
		height = height $ wmap A.! (x, y)})]
	nears =
		filter ((\(x', y') -> x' >= 0 && y' >= 0 && x' <= maxX && y' <= maxY) &&&
		((Empty ==) . terrain . (wmap A.!)) &&&
		((height (wmap A.! (x, y)) >=) . height . (wmap A.!)))
		[(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
	(q, g')= randomR (0.0, 1.0) g

addRivers :: Int -> MapGen -> MapGen
addRivers cnt mgen g = foldr ($) (wmap, g3) $ zipWith addRiver xs ys where
	(wmap, g1) = mgen g
	(gx, g2) = split g1
	(gy, g3) = split g2
	xs = take cnt $ randomRs (0, maxX) gx
	ys = take cnt $ randomRs (0, maxY) gy

addRiversToGen :: Int -> HeiGen -> MapGen
addRiversToGen n = addRivers n . mapGenFromHeightGen

addSwamps :: Int -> A.Array (Int, Int) Int -> A.Array (Int, Int) Cell
addSwamps maxh = ((\x -> Cell {height = x, terrain =
	if x <= maxh then Water else Empty}) <$>)

addSwampsToGen :: Int -> HeiGen -> MapGen
addSwampsToGen maxh hgen g = (addSwamps maxh heis, g') where
	(heis, g') = hgen g

