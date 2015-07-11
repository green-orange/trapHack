module Forgotten where

import Data
import AI
import Random
import Parts
import Stuff
import DataMonster
import DataObject
import DataDef
import Utils4mon

import System.Random (StdGen, randomR, randoms, split)
import Data.Map (fromList)

applyIf :: (a -> a) -> Bool -> a -> a
applyIf f c = if c then f else id

getForgottenBeast :: MonsterGen
getForgottenBeast g = (Monster {
	ai = AI newAI,
	parts = zipWith ($) newParts [0..],
	name = "Forgotten beast",
	stddmg = newDmg,
	inv = newInv,
	slowness = newSlow,
	time = newSlow,
	res = map (const 0) (getAll :: [Elem]),
	intr = map (const 0) (getAll :: [Intr]),
	temp = startTemps,
	idM = 21,
	xp = 1
	}, g5) where
		(newAI, g1) = forgottenAI g
		(newParts, g2) = forgottenParts g1
		(newDmg, g3) = forgottenDmg g2
		(newInv, g4) = forgottenInv g3
		(newSlow, g5) = forgottenSlowness g4

forgottenAI :: StdGen -> (AIrepr, StdGen)
forgottenAI g = (AIrepr {
	mods = map fst $ filter snd $ zip mODSAI bools,
	attackIfCloseMode = Just (elem', dist),
	aipure = StupidAI
}, g'') where
	bools :: [Bool]
	p, q :: Float
	bools = randoms g
	(p, g') = randomR (0.0, 1.0) g
	(q, g'') = randomR (0.0, 1.0) g'
	dist = 1 + inverseSquareRandom p
	elem' = toEnum $ uniform q 0 $ fromEnum (maxBound :: Elem)
	

forgottenParts :: StdGen -> ([Int -> Part], StdGen)
forgottenParts g = (rez, g') where
	qs :: [Float]
	(g', g'') = split g
	qs = randoms g'
	counts = map inverseSquareRandom qs
	partgens = concat $ zipWith replicate counts $ map getPart [0..kINDS]
	qs' = randoms g''
	hps = map ((*10) . inverseSquareRandom) qs'
	rez = zipWith3 ($) partgens (cycle [3, 2, 1]) hps
	
forgottenDmg :: StdGen -> (((Int, Int), Float), StdGen)
forgottenDmg g = (((cnt, dice), failProb), g3) where
	p, q, r :: Float
	(p, g1) = randomR (0.0, 1.0) g
	(q, g2) = randomR (0.0, 1.0) g1
	(r, g3) = randomR (0.0, 1.0) g2
	cnt = 1 + inverseSquareRandom p
	dice = 2 + inverseSquareRandom q
	failProb = 0.5 * r
	
forgottenSlowness :: StdGen -> (Int, StdGen)
forgottenSlowness = randomR (70, 130)

forgottenInv :: InvGen
forgottenInv g = (fromList $ zip alphabet $ filter ((>0) . snd)
	$ zip sTACKABLE nums, g) where
	nums = map ((`div` 3) . inverseSquareRandom) $ randoms g
