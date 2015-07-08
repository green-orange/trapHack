module DataMonster where

import DataDef
import DataObject

import qualified Data.Map as M
import System.Random (StdGen)

type MonsterGen = StdGen -> (Monster, StdGen)

units :: World -> M.Map (Int, Int) Monster
units = list . units'

mapU :: ((Int, Int) -> Monster -> Monster) -> Units -> Units
mapU f uns = uns 
	{list = M.mapWithKey f $ list uns,
	getFirst' = f (xF uns, yF uns) $ getFirst' uns}
	
insertU :: (Int, Int) -> Monster -> Units -> Units
insertU k m uns = uns {list = M.insert k m $ list uns}

deleteU :: (Int, Int) -> Units -> Units
deleteU k uns = uns {list = M.delete k $ list uns}

emptyInv :: InvGen
emptyInv g = (M.empty, g)

isPlayer :: Monster -> Bool
isPlayer mon = case ai mon of
	You -> True
	AI _ -> False
