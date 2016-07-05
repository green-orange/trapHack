module IO.Texts where

import Data.Define

-- | get a message about zapping
msgWand :: String -> String -> String
msgWand title' name' = 
	case title' of
		"wand of striking" -> prefixPast ++ "struck!"
		"wand of stupidity" -> name' ++ " feel" ++ end ++ " stupid!"
		"wand of speed" -> prefix ++ "suddenly moving faster!"
		"wand of radiation" -> prefix ++ "infected by radiation!"
		"wand of psionic blast" -> name' ++ " feel" ++ end ++ 
			" that " ++ (if isYou then "your" else "its") ++ " brains melt!"
		"wand of poison" -> prefixPast ++ "poisoned!"
		"wand of slowing" -> prefix ++ "suddenly moving slowly!"
		"wand of stun" -> prefix ++ "stunned!"
		_ -> error "unknown wand"
	where
	isYou = name' == "You"
	prefix = if isYou then "You are " else name' ++ " is "
	prefixPast = if isYou then "You were " else name' ++ " was "
	end = if isYou then "" else "s"

-- | converts number of a wave when you die to a string
numToStr :: Int -> String
numToStr t
	| t == 1 = "first"
	| t == 2 = "second"
	| t == 3 = "third"
	| t == 4 = "forth"
	| t == 5 = "fifth"
	| t == 6 = "sixth"
	| t == 7 = "seventh"
	| t == 8 = "eighth"
	| t == 9 = "ninth"
	| t == 10 = "tenth"
	| t == 11 = "eleventh"
	| t == 12 = "twelfth"
	| t == 13 = "thirteenth"
	| t == 14 = "fourteenth"
	| t == 15 = "fifteenth"
	| t == 16 = "sixteenth"
	| t == 17 = "seventeenth"
	| t == 18 = "eighteenth"
	| t == 19 = "nineteenth"
	| t == 20 = "twentieth"
	| t == 30 = "thirtieth"
	| t == 40 = "fortieth"
	| t == 50 = "fiftieth"
	| t == 60 = "sixtieth"
	| t == 70 = "seventieth"
	| t == 80 = "eigthieth"
	| t == 90 = "ninetieth"
	| t < 100 = decToStr (div t 10) ++ "-" ++ numToStr (mod t 10)
	| otherwise = show t

-- | converts decimals to 'numToStr'
decToStr :: Int -> String
decToStr t
	| t == 2 = "twenty"
	| t == 3 = "thirty"
	| t == 4 = "forty"
	| t == 5 = "fifty"
	| t == 6 = "sixty"
	| t == 7 = "seventy"
	| t == 8 = "eighty"
	| t == 9 = "ninety"
	| otherwise = error "wrong number of tens"

-- | special attack verb from 'Elem'
attackName :: Elem -> String
attackName Fire = "burn"
attackName Poison' = "poison"
attackName Cold = "freeze"

{-Illegal actions-}
msgSmallScr, msgNECell, msgUnseenCell, msgCantSpawnGC, msgIncStep,
	msgNoItem, msgNotDir, msgNotTrap, msgTrapOverTrap, 
	msgCantUntrap, msgNoWeapAppMiss, msgDropEquipped, msgFullInv, 
	msgWrongBind, msgRepeatedBind, msgUnkAct, msgNaN, msgNotEnough,
	msgUnkRep, msgTooHigh, msgUnkOpt, msgCantDig, msgWater :: String
msgNeedArms, msgDontKnow, msgNoCharge :: String -> String

msgSmallScr = "Your screen is too small."
msgNeedArms s = "You need arms to " ++ s ++ "!"
msgDontKnow s = "You don't know how to " ++ s ++ " it!"
msgNECell = "This cell doesn't exist!"
msgUnseenCell = "You can't see this cell!"
msgCantSpawnGC = "There is no place for the garbage collector!"
msgIncStep = "Incorrect step!"
msgNoItem = "You haven't this item!"
msgNotDir = "It's not a direction!"
msgNoCharge str = "This " ++ str ++ " has no charge!"
msgNotTrap = "It's not a trap!"
msgTrapOverTrap = "You can't set a trap over another trap!"
msgCantUntrap = "It's nothing to untrap here!"
msgNoWeapAppMiss = "You have no weapon appropriate to this missile!"
msgDropEquipped = "You can't drop the item which you have equipped!"
msgFullInv = "You knapsack is full!"
msgWrongBind = "This objects doesn't intended to this part!"
msgRepeatedBind = "This item is already bound to some part!"
msgUnkAct = "Unknown action: "
msgNaN = "This is not a number!"
msgNotEnough = "You don't have enough items!"
msgUnkRep = "Unknown recipe!"
msgTooHigh = "You can not rise so high!"
msgUnkOpt = "Unknown option!"
msgCantDig = "You can't dig here!"
msgWater = "You can't move through water!"

{-Interaction messages-}
msgAskName, msgMore, msgAskDir, msgCheater, msgAsk, msgInfo, 
	msgConfirmCall, msgSaved, msgAskLoad, msgLoadErr, msgGameErr, 
	msgPutSize :: String
msgWelcome :: String -> String

msgWelcome username = "Welcome to the trapHack, " ++ username ++ "."
msgAskName = "What's your name?"
msgMore = "--MORE--"
msgAskDir = "In what direction?"
msgCheater = "You are cheater!"
msgAsk = "What do you want to "
msgInfo = "Pick an object and press ."
msgConfirmCall = "Do you really want to call upon the next wave? (y/N)"
msgSaved = "Game saved!"
msgAskLoad = "Do you want to load saved game? (y/N)"
msgLoadErr = "Loading error! File is corrupted."
msgGameErr = "Game error! If you've recently loaded the game, " ++
	"maybe the file was damaged. Otherwise, report the bug to the developer. "
msgPutSize = "Enter the size of one pile."

{-Headers-}
msgHeaderInv, msgHeaderEquip, msgHeaderBind, msgHeaderCraft :: String
msgHeaderPickDrop :: String -> String

msgHeaderInv = "Your inventory: (press Enter, Space or Escape to close it)"
msgHeaderEquip = "Choose an item or press - to choose nothing"
msgHeaderPickDrop word = "What do you want to " ++ word 
	++ " up? (press Enter to finish)"
msgHeaderBind = "Choose your part to bind."
msgHeaderCraft = "Choose a recipe."

{-Exit messages-}
msgQuitOnStart :: String
msgYouDie, msgQuit :: Int -> String

msgYouDie n = "You died on the " ++ numToStr n ++ " wave."
msgQuitOnStart = "You quit. Do not pass go. Do not collect 200 zorkmids."
msgQuit n = "You quit on the " ++ numToStr n ++ " wave."

{-Attack-}
msgMiss, msgAttack, msgHitMissile :: String

msgMiss = " missed!"
msgAttack = " hit"
msgHitMissile = " hits "

{-Elemental attacks-}
msgFire, msgFireYou, msgPoison, msgPoisonYou :: String

msgFire = " is in fire!"
msgFireYou = "You are in fire!"
msgPoison = " was poisoned!"
msgPoisonYou = "You were poisoned!"

{- Options -}
msgOptColorHei, msgOptColorMon, msgOptNoHei, msgOptColorHeiAbs :: String

msgOptColorHei = "a - map is colored by relative height, monsters are white."
msgOptColorMon = "b - monsters are colored, height shown by white numbers."
msgOptNoHei = "c - height doesn't shown"
msgOptColorHeiAbs = "d - map is colored by absolute height, monsters are white."

{-Other-}
msgSafety, msgTrollDeath, msgDrownYou, msgDrown :: String
msgLanding :: Int -> String
msgLevelUp, msgWE :: String -> String
msgTeleport :: String -> String
msgCraft :: String -> String -> String
msgGetStones :: Int -> String

msgLanding n = "Squad #" ++ show n ++ " landed around you!"
msgSafety = "You suddenly find yourself in a new world!"
msgTrollDeath = "Troll turned into a rock."
msgLevelUp t = 
	if t == "You"
	then "You become more experienced!"
	else t ++ " seems to become more experienced!"
msgWE fun = "weird error in function '" ++ fun ++ "'"
msgTeleport "You" = "You teleported to another location!"
msgTeleport str = str ++ " was teleported!"
msgCraft n t = 
	if n == "You"
	then "You successfully craft " ++ t ++ "!"
	else n ++ " successfully crafts " ++ t ++ "!"
msgGetStones 1 = "You get a stone!"
msgGetStones n = "You get " ++ show n ++ " stones!"
msgDrownYou = "You are drowning!"
msgDrown = " sinks!"

-- | put 'weird error' in given function
putWE :: String -> a
putWE = error . msgWE
