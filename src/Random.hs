module Random where

import DataDef

import System.Random

intToFloat :: Int -> Float
intToFloat = fromIntegral

dicesWithoutFail :: (Int, Int) -> World -> (Int, StdGen)
dicesWithoutFail (cnt, dice) world = (dmgNew, stdGenNew) where
	(diceNew, stdGenNew) = randomR (1, dice) $ stdgen world
	dmgNew = diceNew * cnt

dices :: (Int, Int) -> Float -> StdDmg
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
	inverseSquareRandom' p' n = 
		let
			sqr :: Float
			sqr = intToFloat $ n * n
			bound = (1.0 / sqr) * 6.0 / (pi * pi) in
		if p' <= bound
		then n
		else inverseSquareRandom' (p' - bound) (n + 1)
		
inverseSquareList :: [Float]
inverseSquareList = map (/(pi*pi/6.0*0.999)) $ inverseSquareList' 0.0 1 where
	inverseSquareList' q n = new : inverseSquareList' new (n + 1) where
		new = q + 1.0 / (intToFloat n * intToFloat n) 
		
uniform :: Float -> Int -> Int -> Int
uniform p l r = (+) l $ floor $ (*) p $ intToFloat $ r - l + 1

uniformFromList :: Float -> [a] -> a
uniformFromList q xs = xs !! n where n = uniform q 0 $ length xs - 1

frac :: Float -> Float
frac q = (-) q ((intToFloat $ floor q) :: Float)

