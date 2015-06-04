module Utils4all where

import Data

bODY = 0 :: Int
hEAD = 1 :: Int
lEG  = 2 :: Int
aRM  = 3 :: Int
wING = 4 :: Int
pAW  = 5 :: Int

eMPTY    = 0 :: Int
bEARTRAP = 1 :: Int

effectiveSlowness :: Monster -> Int
effectiveSlowness mon =
	div (slowness mon) $ 1 + (length $ filter isLowerLimb $ parts mon)


isLowerLimb :: Part -> Bool
isLowerLimb p = (kind p == lEG) || (kind p == wING) || (kind p == pAW)

isUpperLimb :: Part -> Bool
isUpperLimb p = (kind p == aRM) || (kind p == wING) || (kind p == pAW)

getFirst :: World -> Monster
getFirst world =
	if null $ units world
	then error "No monsters"
	else third $ head $ units world
	
first :: (a,b,c) -> a
first (x,_,_) = x

second :: (x,b,c) -> b
second (_,x,_) = x

third :: (a,b,c) -> c
third (_,_,x) = x
	
partToStr :: Int -> String
partToStr x
	| x == bODY = "Body"
	| x == hEAD = "Head"
	| x == lEG  = "Leg"
	| x == aRM  = "Arm"
	| x == wING = "Wing"
	| x == pAW  = "Paw"
	| otherwise = error "unknown part"

titleShow :: Object -> String
titleShow (Potion title _) = title
titleShow (Wand title _ _ charge) = title ++ " (" ++ show charge ++ ")"
titleShow (Trap title _) = title
titleShow _ = error "unknown object"

isStackable :: Object -> Bool
isStackable obj = obj == obj

