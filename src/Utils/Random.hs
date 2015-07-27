module Utils.Random where

import Data.Define

import System.Random
import Data.Functor ((<$>))

-- | fromIntegral specifcated to this signature
intToFloat :: Int -> Float
intToFloat = fromIntegral

-- | dicesWithoutFail (cnt, dice) is cnt 'd' dice in D&D-like notation
dicesWithoutFail :: (Int, Int) -> World -> (Int, StdGen)
dicesWithoutFail (cnt, dice) world = (dmgNew, stdGenNew) where
	(diceNew, stdGenNew) = randomR (1, dice) $ stdgen world
	dmgNew = diceNew * cnt

-- | cnt 'd' dice with fixed fail probability
dices :: (Int, Int) -> Float -> StdDmg
dices xs p world = (rez, stdGenNew) where
	(dmg, stdGenOld) = dicesWithoutFail xs world
	(rnd, stdGenNew) = randomR (0.0, 1.0) stdGenOld
	rez =
		if rnd < p
		then Nothing
		else Just dmg

-- | probability of n is 6/(pi^2*n^2) 
inverseSquareRandom :: Float -> Int
inverseSquareRandom p = inverseSquareRandom' (0.999999 * p) 1 where
	inverseSquareRandom' :: Float -> Int -> Int
	inverseSquareRandom' p' n = 
		let
			sqr :: Float
			sqr = intToFloat $ n * n
			bound = (1.0 / sqr) * 6.0 / (pi * pi) in
		if p' <= bound
		then n
		else inverseSquareRandom' (p' - bound) (n + 1)

-- | list of partial sums of [1/n^2]
inverseSquareList :: [Float]
inverseSquareList = (/(pi*pi/6.0*0.999)) <$> inverseSquareList' 0.0 1 where
	inverseSquareList' q n = new : inverseSquareList' new (n + 1) where
		new = q + 1.0 / (intToFloat n * intToFloat n) 

-- | discrete uniform distribution from l to r 
uniform :: Float -> Int -> Int -> Int
uniform p l r = (+) l $ floor $ (*) p $ intToFloat $ r - l + 1

-- | random element from list with discrete uniform distribution
uniformFromList :: Float -> [a] -> a
uniformFromList q xs = xs !! n where n = uniform q 0 $ length xs - 1

-- | fraction part of float; x - [x]
frac :: Float -> Float
frac q = (-) q ((intToFloat $ floor q) :: Float)

-- | split one StdGen to a list of them
splitList :: StdGen -> [StdGen]
splitList g = g' : splitList g'' where (g', g'') = split g
