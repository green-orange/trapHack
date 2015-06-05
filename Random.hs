module Random where

import Data

import System.Random

dicesWithoutFail :: [(Int, Int)] -> World -> (Int,StdGen) -- [(n_i, k_i)] -> Sum(n_i d (k_i))
dicesWithoutFail [] world = (0, stdgen world)
dicesWithoutFail (x:xs) world = (dmgNew, stdGenNew) where
	(dmgOld, stdGenOld) = dicesWithoutFail xs world
	(diceNew, stdGenNew) = randomR (1, snd x) stdGenOld
	dmgNew = diceNew * (fst x)

dices :: [(Int,Int)] -> Float -> World -> (Maybe Int, StdGen)
dices xs p world = (rez, stdGenNew) where
	(dmg, stdGenOld) = dicesWithoutFail xs world
	(rnd, stdGenNew) = randomR (0.0, 1.0) stdGenOld
	rez =
		if rnd < p
		then Nothing
		else Just dmg
		
inverseSquareRandom :: Float -> Int
inverseSquareRandom p = inverseSquareRandom' (0.999999 * p) 1 where
	inverseSquareRandom' :: Float -> Int -> Int
	inverseSquareRandom' p n = 
		let
			sqr :: Float
			sqr = fromIntegral $ n * n
			bound = (1.0 / sqr) * 6.0 / (pi * pi) in
		if p <= bound
		then n
		else inverseSquareRandom' (p - bound) (n + 1)
