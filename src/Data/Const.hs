module Data.Const where

maxX, maxY, xSight, ySight :: Int
-- | maximum value of x coordinate in the world
maxX = 99
-- | maximum value of y coordinate in the world
maxY = 59
-- | maximum value of x for what you can see
xSight = 20
-- | maximum value of y for what you can see
ySight = 10

-- | get all values of given enumerable and bounded type
getAll :: (Bounded a, Enum a) => [a]
getAll = [minBound..maxBound]

alphabet, notAlphabet :: String
-- | latin alphabet: a..zA..Z
alphabet = ['a'..'z'] ++ ['A'..'Z']
-- | all symbols after latin alphabet
notAlphabet = ['{'..]

-- | number of clear strings at the top of a screen
shiftDown :: Int
shiftDown = 5

-- | IDs for different body parts
bODY, hEAD, lEG, aRM, wING, pAW, kINDS, mAIN :: Int

bODY = 0
hEAD = 1
lEG  = 2
aRM  = 3
wING = 4
pAW  = 5
kINDS = pAW

mAIN = 32
