module Data.Monster where

import Data.Define

import qualified Data.Map as M
import System.Random (StdGen)

-- | random monster generator
type MonsterGen = StdGen -> (Monster, StdGen)

-- | interface to get list of units from the world
units :: World -> M.Map (Int, Int) Monster
units = list . units'

-- | 'fmap' analogue for 'Units'
mapU :: ((Int, Int) -> Monster -> Monster) -> Units -> Units
mapU f uns = uns 
	{list = M.mapWithKey f $ list uns,
	getFirst' = f (xF uns, yF uns) $ getFirst' uns}

-- | insert a new units with given coords
insertU :: (Int, Int) -> Monster -> Units -> Units
insertU k m uns = uns {list = M.insert k m $ list uns}

-- | delete a new units with given coords
deleteU :: (Int, Int) -> Units -> Units
deleteU k uns = uns {list = M.delete k $ list uns}

-- | get empty inventory
emptyInv :: InvGen
emptyInv g = (M.empty, g)

-- | is a monster under player control
isPlayer :: Monster -> Bool
isPlayer mon = case ai mon of
	You -> True
	AI _ -> False
