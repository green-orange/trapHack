module MapGen where

import Data.Define
import Data.Const
import Data.World
import Utils.Random

import System.Random
import qualified Data.Array as A
import Data.Functor ((<$>))
import Control.Arrow (first)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | instance to add, multiply etc functions from somewhere to numbers
instance Num b => Num (a -> b) where
	(f + g) x = f x + g x
	(f - g) x = f x - g x
	(f * g) x = f x * g x
	fromInteger = const . fromInteger
	(abs f) x = abs $ f x
	(signum f) x = signum $ f x

-- | converts HeiGenType (enumerable type) to height generator function
runHei :: HeiGenType -> HeiGen
runHei (Sines n) = getSineMap n
runHei Random = getRandomHeis
runHei (Hills n) = getHillMap n
runHei (Mountains n) = getMountainOrValleyMap n
runHei (Flat n) = getFlatMap n
runHei (DiamondSquare) = getDiamondSquareMap
runHei (Voronoi n) = getVoronoiMap n

-- | converts given type of water and height generator to a map generator
runWater :: Water -> HeiGen -> MapGen
runWater NoWater = mapGenFromHeightGen
runWater (Rivers n) = addRiversToGen n
runWater (Swamp n) = addSwampsToGen n

-- | add given type of traps to map generator
runTraps :: TrapMap -> MapGen -> MapGen
runTraps NoTraps = id
runTraps (Bonfires n) = foldr (.) id $ replicate n $ addRandomTerr Bonfire
runTraps (MagicMap n) = foldr (.) id $ replicate n $ addRandomTerr MagicNatural

-- | converts MapGenType (enumerable type) to map generator function
runMap :: MapGenType -> MapGen
runMap (MapGenType heigen avg water traps) = runTraps traps $ runWater water
	$ foldr (.) (limit *. runHei heigen) $ replicate avg $ first averaging

-- | return map generator with given height generator and without water
pureMapGen :: HeiGenType -> MapGenType
pureMapGen heigen = MapGenType heigen 0 NoWater NoTraps

-- | generator of the world map
type MapGen = StdGen -> (A.Array (Int, Int) Cell, StdGen)
-- | generator of the height map
type HeiGen = StdGen -> (A.Array (Int, Int) Int, StdGen)

-- | apply first argument to the first element of the result of second argument
infixr 9 *.
(*.) :: (a -> c) -> (b -> (a, b)) -> b -> (c, b)
(f *. g) x = (f rez, x') where
	(rez, x') = g x

-- | heights < 0 reduced to 0, heights > 9 reduced to 9
limit :: A.Array (Int, Int) Int -> A.Array (Int, Int) Int
limit = fmap $ max 0 . min 9

-- | smoothes the map by taking the average of neighboring cells
averaging :: A.Array (Int, Int) Int -> A.Array (Int, Int) Int
averaging arr = A.array ((0, 0), (maxX, maxY))
	[((x, y), avg x y) | x <- [0..maxX], y <- [0..maxY]] where
	d = [-1..1]
	avg x y = (2 * (arr A.! (x, y)) + sum ((arr A.!) <$> nears)) 
		`div` (2 + length nears) where
		nears = [(x + dx, y + dy) | dx <- d, dy <- d,
			isCell (x + dx) (y + dy)]

-- | converts height map to full map without any obstacles
mapFromHeights :: A.Array (Int, Int) Int -> A.Array (Int, Int) Cell
mapFromHeights = fmap (\h -> Cell {terrain = Empty, height = h})

-- | converts height generator to map generator without any obstacles
mapGenFromHeightGen :: HeiGen -> MapGen
mapGenFromHeightGen hgen = mapFromHeights *. hgen

-- | get a map with equal heights
getFlatMap :: Int -> HeiGen
getFlatMap n g = (A.listArray ((0, 0), (maxX, maxY)) 
	[n, n..], g)

-- | get a map with random heights
getRandomHeis :: HeiGen
getRandomHeis g = (A.listArray ((0, 0), (maxX, maxY)) 
	$ randomRs (0, 9) g', g'') where
	(g', g'') = split g

-- | get a parabolic hill with radius r and center (x0, y0)
getHill :: Float -> Float -> Float -> (Int, Int) -> Float
getHill r x0 y0 (x, y) = max 0 $ r ** 2 - (fromIntegral x - x0) ** 2 - (fromIntegral y - y0) ** 2

-- | get sum of n random hills
getSumHills :: Int -> StdGen -> ((Int, Int) -> Float, StdGen)
getSumHills n g = (f, g3) where
	(gr, g1) = split g
	(gx, g2) = split g1
	(gy, g3) = split g2
	rs = randomRs (4.0, 5.0) gr
	xs = randomRs (0.0, fromIntegral maxX) gx
	ys = randomRs (0.0, fromIntegral maxY) gy
	f = sum $ take n $ zipWith3 getHill rs xs ys

-- | get a sine wave with parameters a and b
getSineWave :: Float -> Float -> (Int, Int) -> Float
getSineWave a b (x, y) = sin $ a * fromIntegral x + b * fromIntegral y

-- | get sum of n random sine waves
getSumSines :: Int -> StdGen -> ((Int, Int) -> Float, StdGen)
getSumSines n g = (f, g2) where
	(ga, g1) = split g
	(gb, g2) = split g1
	as = randomRs (0.1, 1.0) ga
	bs = randomRs (0.1, 1.0) gb
	f = sum $ take n $ zipWith getSineWave as bs

-- | get a mountain or valley (valley if true)
getMountainOrValley :: Bool -> Float -> Float -> (Int, Int) -> Float
getMountainOrValley t x0 y0 (x, y) = (* 2) $ getType $ exp $ negate $ sqrt 
	$ (x0 - fromIntegral x) ** 2 + (y0 - fromIntegral y) ** 2 where
	getType = if t then (-) 0.004 else id

-- | get sum of n random mountains or valleys
getSumMountainOrValley :: Int -> StdGen -> ((Int, Int) -> Float, StdGen)
getSumMountainOrValley n g = (f, g3) where
	(gx, g1) = split g
	(gy, g2) = split g1
	(gb, g3) = split g2
	xs = randomRs (0.0, fromIntegral maxX) gx
	ys = randomRs (0.0, fromIntegral maxY) gy
	bs = randomRs (False, True) gb
	f = sum $ take n $ zipWith3 getMountainOrValley bs xs ys
	
-- | get a map with random mountains like
-- exp (sqrt ((x - x0) ^ 2 + (y - y0) ^ 2)) and symmetric valleys
getMountains :: Int -> HeiGen
getMountains n gen = (A.array ((0, 0), (maxX, maxY))
	[((x, y), sumLand (x, y)) | x <- [0..maxX], y <- [0..maxY]], g') where
	(g, g') = split gen
	(gx, gy)= split g
	xs = randomRs (0, maxX) gx
	ys = randomRs (0, maxY) gy
	mnts = take n $ zipWith getMnt xs ys
	vlls = take n $ drop n $ zipWith getVll xs ys 
	getMnt, getVll :: Int -> Int -> (Int, Int) -> Float
	getMnt xMnt yMnt (x, y) = (* 2) $ exp $ negate $ sqrt 
		$ fromIntegral $ (xMnt - x) ^ (2 :: Int) + (yMnt - y) ^ (2 :: Int)
	getVll xMnt yMnt (x, y) = (* 2) $ negate $ exp $ negate $ sqrt 
		$ fromIntegral $ (xMnt - x) ^ (2 :: Int) + (yMnt - y) ^ (2 :: Int)
	sumMnts = floor . sum mnts
	sumVlls = floor . sum vlls
	sumLand = sumMnts + sumVlls

-- | get heightmap from a height function, cut-offs are for normalizeA
getMapFromFun :: (Float, Float) -> ((Int, Int) -> Float) -> A.Array (Int, Int) Int
getMapFromFun cuts f = normalizeA cuts $ A.array ((0, 0), (maxX, maxY))
	[((x, y), f (x, y)) | x <- [0..maxX], y <- [0..maxY]]

-- | normalize array to [0, 9] with given cut-offs
normalizeA :: (Float, Float) -> A.Array (Int, Int) Float -> A.Array (Int, Int) Int
normalizeA (minC, maxC) a = norm <$> a where
	maxA = maximum $ A.elems a
	minA = minimum $ A.elems a
	norm x = max 0 $ min 9 $ floor $ (x - minA) / (maxA - minA) * (maxC - minC) - minC

-- | get map with default cut-offs
getMapFromFunDef :: ((Int, Int) -> Float) -> A.Array (Int, Int) Int
getMapFromFunDef = getMapFromFun (-1.0, 11.0)

-- | height generator for hills
getHillMap :: Int -> HeiGen
getHillMap n = getMapFromFunDef *. getSumHills n

-- | height generator for sine waves
getSineMap :: Int -> HeiGen
getSineMap n = getMapFromFunDef *. getSumSines n

-- | height generator for mountains or valleys
getMountainOrValleyMap :: Int -> HeiGen
getMountainOrValleyMap n = getMapFromFunDef *. getSumMountainOrValley n

-- | add one river starts from (x, y) and flowing down
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
		filter (uncurry isCell &&&
		((Empty ==) . terrain . (wmap A.!)) &&&
		((height (wmap A.! (x, y)) >=) . height . (wmap A.!)))
		[(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
	(q, g')= randomR (0.0, 1.0) g

-- | add 'cnt' rivers
addRivers :: Int -> MapGen -> MapGen
addRivers cnt mgen g = foldr ($) (wmap, g3) $ zipWith addRiver xs ys where
	(wmap, g1) = mgen g
	(gx, g2) = split g1
	(gy, g3) = split g2
	xs = take cnt $ randomRs (0, maxX) gx
	ys = take cnt $ randomRs (0, maxY) gy

-- | add 'n' rivers to height generator
addRiversToGen :: Int -> HeiGen -> MapGen
addRiversToGen n = addRivers n . mapGenFromHeightGen

-- | add swamps with given depth
addSwamps :: Int -> A.Array (Int, Int) Int -> A.Array (Int, Int) Cell
addSwamps maxh = ((\x -> Cell {height = x, terrain =
	if x <= maxh then Water else Empty}) <$>)

-- | add swamps to height generator
addSwampsToGen :: Int -> HeiGen -> MapGen
addSwampsToGen maxh hgen g = (addSwamps maxh heis, g') where
	(heis, g') = hgen g

-- | add given terrain to a random place if this place is 'Empty'
addRandomTerr :: Terrain -> MapGen -> MapGen
addRandomTerr terr mgen g = 
	if terrain cell == Empty
	then (wmap A.// [((x, y), cell {terrain = terr})], g3)
	else (wmap, g3)
	where
	(x, g1) = randomR (0, maxX) g
	(y, g2) = randomR (0, maxY) g1
	(wmap, g3) = mgen g2
	cell = wmap A.! (x, y)

-- | minimal power of 2 that is more than maxX and maxY
diamondSquareSize :: Int
diamondSquareSize = 128

-- | parameter for algorithm, also need to be power of 2
blockSize :: Int
blockSize = 16

-- | list of odd multiples of n
coordsOdd :: Int -> [Int]
coordsOdd n = [n, 3 * n .. diamondSquareSize - n]

-- | list of even multiples of n
coordsEven :: Int -> [Int]
coordsEven n = [0, 2 * n .. diamondSquareSize]

-- | maximum moise on first step
noiseCoeff :: Float
noiseCoeff = 10.0

-- | get bound for nois by iteration
getBound :: Int -> Float
getBound n = noiseCoeff * fromIntegral n / fromIntegral blockSize

-- | one 'diamond' step of algorithm
addDiamonds :: Int -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addDiamonds n p = foldr (addDiamond n) p [(x, y) | x <- coordsOdd n, y <- coordsOdd n] where

-- | add one 'diamond' with given coords
addDiamond :: Int -> (Int, Int) -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addDiamond n (x, y) (m, g) = (M.insert (x, y) newVal m, g') where
	newVal = max 0.0 $ min 10.0 $ (+) noise $ flip (/) 4.0 $ sum 
		$ fromMaybe 5.0 . flip M.lookup m <$>
		[(x - n, y - n), (x - n, y + n), (x + n, y - n), (x + n, y + n)]
	(noise, g') = randomR (-bound, bound) g
	bound = getBound n

-- | one 'vertical square' step of algorithm
addSquaresV :: Int -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addSquaresV n p = foldr (addSquareV n) p [(x, y) | x <- coordsEven n, y <- coordsOdd n] where

-- | add one 'vertical square' with given coords
addSquareV :: Int -> (Int, Int) -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addSquareV n (x, y) (m, g) = (M.insert (x, y) newVal m, g') where
	newVal = max 0.0 $ min 10.0 $ (+) noise $ flip (/) 2.0 $ sum 
		$ fromMaybe 5.0 . flip M.lookup m <$> [(x, y - n), (x, y + n)]
	(noise, g') = randomR (-bound, bound) g
	bound = getBound n

-- | one 'horizontal square' step of algorithm
addSquaresH :: Int -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addSquaresH n p = foldr (addSquareH n) p [(x, y) | x <- coordsOdd n, y <- coordsEven n] where

-- | add one 'horizontal square' with given coords
addSquareH :: Int -> (Int, Int) -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addSquareH n (x, y) (m, g) = (M.insert (x, y) newVal m, g') where
	newVal = max 0.0 $ min 10.0 $ (+) noise $ flip (/) 2.0 $ sum 
		$ fromMaybe 5.0 . flip M.lookup m <$> [(x - n, y), (x + n, y)]
	(noise, g') = randomR (-bound, bound) g
	bound = getBound n

-- | run step with distance n
diamondSquareStep :: Int -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
diamondSquareStep 0 = id
diamondSquareStep n = diamondSquareStep (n `div` 2) . addSquaresV n . addSquaresH n . addDiamonds n

-- | random height by given coords
addRandomHei :: (Int, Int) -> (M.Map (Int, Int) Float, StdGen) -> (M.Map (Int, Int) Float, StdGen)
addRandomHei p (m, g) = (M.insert p newVal m, g') where
	(newVal, g') = randomR (0.0, 10.0) g

getStartMap :: StdGen -> (M.Map (Int, Int) Float, StdGen)
getStartMap g = foldr addRandomHei (M.empty, g) [(x, y) |
	x <- coordsEven blockSize, y <- coordsEven blockSize]

-- | generate a map using 'diamond square' algorithm
diamondSquare :: StdGen -> (M.Map (Int, Int) Float, StdGen)
diamondSquare = diamondSquareStep blockSize . getStartMap where

-- | wrapped height generator
getDiamondSquareMap :: HeiGen
getDiamondSquareMap g = (A.array ((0, 0), (maxX, maxY)) $ M.toList $
	M.filterWithKey isGoodKey $ floor <$> m, g') where
	(m, g') = diamondSquare g
	isGoodKey key _ = uncurry isCell key

-- | get height by list of centers and coords
getVoronoiHeight :: [(Float, Float)] -> (Int, Int) -> Float
getVoronoiHeight centers (x, y) = minimum $ map
	(dist (fromIntegral x, fromIntegral y)) centers where
		dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

-- | get 'Voronoi diagram' map
getVoronoiMap :: Int -> HeiGen
getVoronoiMap cnt g = (getMapFromFun (0.0, 20.0) $ getVoronoiHeight $ zip xs ys, g2) where
	(gx, g1) = split g
	(gy, g2) = split g1
	xs = take cnt $ randomRs (0.0, fromIntegral maxX) gx
	ys = take cnt $ randomRs (0.0, fromIntegral maxY) gy
	
