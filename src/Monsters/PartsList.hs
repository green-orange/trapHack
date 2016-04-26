module Monsters.PartsList where

import Data.Define
import Monsters.Parts

partsHom, partsBtl, partsBat, partsHun, partsAcc, partsTrl, partsWrm, 
	partsFlE, partsRDr, partsWDr, partsGDr, partsSpd, partsSol, partsUmH, 
	partsIvy, partsTai, partsGrC, partsGlm, partsRck, partsTre, partsBot, 
	partsBee, partsBsh :: [(Int -> Int -> Part, (Int, Int))]

partsHom = 
	(getBody 1, (10, 30)) :
	(getHead 1, (8, 12))  :
	replicate 2 (getLeg  1, (3, 7)) ++ 
	replicate 2 (getArm  1, (2, 6))

partsBtl = 
	(getBody 1, (10, 20)) :
	(getHead 1, (5, 15))  :
	replicate 2 (getPaw 1, (5, 9)) ++
	replicate 4 (getLeg 1, (2, 8))

partsBat = 
	(getBody 1, (10, 50)) :
	(getHead 1, (5, 35))  :
	replicate 2 (getWing 2, (5, 15))

partsHun = 
	(getBody 1, (10, 30)) :
	(getHead 1, (10, 30)) :
	replicate 2 (getLeg 1, (5, 10)) ++
	replicate 2 (getArm 1, (5, 10))

partsAcc = 
	(getBody 1, (10, 20)) : 
	(getHead 1, (8, 12))  :
	replicate 2(getLeg 1, (3, 7)) ++
	replicate 2 (getArm 1, (2, 6))

partsTrl = 
	(getBody 2, (10, 30)) :
	(getHead 2, (10, 20)) :
	replicate 2 (getLeg 3, (8, 12)) ++
	replicate 2 (getArm 3, (8, 12))

partsWrm = [(getMain 1, (200, 500))]
	
partsFlE = 
	(getMain 2, (10, 40)) :
	replicate 2 (getWing 1, (5, 10))

partsRDr = 
	(getBody 2, (10, 40)) :
	(getHead 2, (10, 30)) :
	replicate 2 (getLeg 1, (5, 15)) ++
	replicate 2 (getWing 3, (5, 15))

partsWDr = 
	(getBody 2, (10, 40)) :
	(getHead 2, (10, 30)) :
	replicate 2 (getLeg 1, (5, 15)) ++
	replicate 2 (getWing 3, (10, 20))

partsGDr = 
	(getBody 2, (10, 40)) :
	(getHead 2, (10, 30)) :
	replicate 2 (getLeg 1, (10, 20)) ++
	replicate 2 (getWing 3, (10, 30))

partsSpd = 
	(getBody 1, (10, 20)) :
	(getHead 1, (5, 15))  :
	replicate 2 (getPaw 1, (5, 8)) ++
	replicate 6 (getLeg 1, (2, 5))

partsSol = 
	(getBody 2, (10, 30)) :
	(getHead 2, (10, 20)) :
	replicate 2 (getLeg 3, (8, 12)) ++
	replicate 2 (getArm 3, (8, 12))

partsUmH = 
	(getBody 2, (10, 20)) :
	(getHead 2, (10, 15)) :
	replicate 2 (getLeg 3, (5, 10)) ++
	replicate 2 (getArm 3, (5, 10))

partsIvy = [(getMain 2, (5, 15))]
		
partsTai = [(getMain 0, (100, 200))]

partsGrC = 
	(getBody 1, (20, 40)) :
	(getHead 1, (10, 30)) :
	replicate 2 (getLeg  1, ( 8, 12)) ++
	replicate 2 (getArm  1, ( 8, 12))

partsGlm = 
	(getBody 1, (10, 30)) :
	(getHead 1, (8, 12))  :
	replicate 2 (getLeg  1, ( 3,  7)) ++
	replicate 2 (getArm  1, ( 2,  6))

partsRck = [(getMain 0, (100, 5000))] 

partsTre = [(getMain 1, (50, 100))]

partsBot = 
	(getBody 1, (10, 15)) :
	(getHead 1, (8, 12))  :
	replicate 2 (getLeg 1, (1, 6)) ++ 
	replicate 2 (getArm 1, (1, 6))

partsBee = 
	(getBody 1, (10, 20)) :
	(getHead 1, (8, 15))  :
	replicate 2 (getWing 2, (2, 8))

partsBsh = partsIvy
