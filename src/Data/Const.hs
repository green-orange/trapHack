module Data.Const where

lol :: a
lol = undefined

maxX, maxY, xSight, ySight :: Int
maxX = 99
maxY = 59
xSight = 20
ySight = 10

getAll :: (Bounded a, Enum a) => [a]
getAll = [minBound..maxBound]

alphabet, notAlphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']
notAlphabet = ['{'..]

shiftDown :: Int
shiftDown = 5

bODY, hEAD, lEG, aRM, wING, pAW, kINDS, mAIN :: Int

bODY = 0
hEAD = 1
lEG  = 2
aRM  = 3
wING = 4
pAW  = 5
kINDS = pAW

mAIN = 32
