module Forgotten where

import Data
import AI
import Random
import Parts
import Stuff

import System.Random (StdGen)
import Control.Monad (join)
import Data.Map (fromList)

applyIf :: (a -> a) -> Bool -> (a -> a)
applyIf f c = if c then f else id

forgottenAI :: Float -> AIfunc
forgottenAI q = foldr ($) pureAI $ zipWith applyIf mODSAI bools where
	bools = randomBools q
	pureAI = attackIfClose dist stupidAI
	dist = 1 + inverseSquareRandom q
		
forgottenParts :: Float -> [Int -> Part]
forgottenParts q = rez where
	kinds :: Float
	kinds = fromIntegral kINDS + 1
	qs = map (frac . (*q) . (/kinds) . fromIntegral) [0..kINDS]
	counts = map inverseSquareRandom qs
	partgens = join $ zipWith replicate counts $ map getPart [0..]
	qs' = map (frac . (*q) . (/ (fromIntegral $ length partgens)) 
		. fromIntegral) [0..length partgens - 1]
	hps = map ((*10) . inverseSquareRandom) qs'
	rez = zipWith3 (($).($)) partgens hps $ cycle $ [3, 2, 1]
	
forgottenDmg :: Float -> World -> (Maybe Int, StdGen)
forgottenDmg q = dices (cnt, dice) failProb where
	cnt = (+) 1 $ inverseSquareRandom $ frac $ q + (1.0 / 3)
	dice = (+) 2 $ inverseSquareRandom $ frac $ q + (2.0 / 3)
	failProb = (*) 0.5 $ frac $ q + pi
	
forgottenSlowness :: Float -> Int
forgottenSlowness q = uniform q 70 130

forgottenInv :: InvGen
forgottenInv q = fromList $ zip alphabet $ filter ((>0) . snd) 
	$ zip sTACKABLE nums where
	nums = map ((`div` 3) . inverseSquareRandom . frac) [q, q + pi..]
	
	
