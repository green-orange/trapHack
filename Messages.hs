module Messages where

import Data
import Changes

titleShow :: Object -> String

titleShow (Wand t _ _ ch) = t ++ " (" ++ show ch ++ ")"
titleShow x = title x

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

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toEnum (fromEnum x - fromEnum 'a' + fromEnum 'A') : xs

ending :: World -> String
ending world =
	if isPlayerNow world
	then " "
	else "s "

addArticle :: String -> String
addArticle str = 
	if str == ""
	then ""
	else if (elem (head str) "aeiouAEIOU")
	then "an " ++ str
	else "a " ++ str

lostMsg :: String -> String -> String
lostMsg monName partName =
	if partName == "Main"
	then ""
	else monName ++ " lost " ++ addArticle partName ++ "."
	
maybeAddMessage :: String -> World -> World
maybeAddMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, yELLOW) w
	else w
	
addNeutralMessage :: String -> World -> World
addNeutralMessage msg w = 
	if isPlayerNow w
	then addMessage (msg, gREEN) w
	else addMessage (msg, yELLOW) w
	
addDefaultMessage :: String -> World -> World
addDefaultMessage msg w = addMessage (msg, dEFAULT) w

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
		_ -> error "unknown wand"
	where
	isYou = name' == "You"
	prefix = if isYou then "You are " else name' ++ " is "
	prefixPast = if isYou then "You were " else name' ++ " was "
	end = if isYou then "" else "s"


