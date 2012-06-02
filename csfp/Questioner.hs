import Data.Maybe
import Data.List
import GHC.IO.Handle
import System.IO

import Parsing
import LogicalForm

yesorno :: Answer -> String
yesorno (Boolean False) = "No"
yesorno (Boolean True)  = "Yes"

takeCourse :: String -> String -> String
takeCourse _ "WH" = "WH"
takeCourse "WH" _ = "WH"
takeCourse _ "YN" = "YN"
takeCourse "YN" _ = "YN"
takeCourse _ "S"  = "S"
takeCourse "S" _  = "S"
takeCourse "Unparseable" _  = "Unparseable"
takeCourse _  _   = error "undefined course, not WH, YN, S, or Unparseable"

pickOne :: String -> String -> String
pickOne "NoAnswer" x   = x
pickOne "[]" x         = x
pickOne x   "NoAnswer" = x
pickOne x       "[]"   = x
pickOne "False" "True" = "True"
pickOne "True" "False" = "True"
pickOne "False" x      = x
pickOne "True"  x      = x
pickOne x   y  = if (x == y) then x
		 	else x ++ ", " ++ y
-- pickOne _       _      = error "undefined eval, not NoAnswer, [], True or False"

main = do
	sentence <- getLine
	let lexed = lexer sentence
	putStrLn $ unwords lexed
	let parselist = case listparses of
		[] -> [Ep]
		otherwise -> listparses
		where listparses = parses sentence 
	hClose stderr
	hDuplicateTo stdout stderr
	let labelFormAnswers =
		map (\p -> (label p,form p,answer p)) parselist where
			label Ep = "Unparseable"
			label p  = catLabel $ t2c p
			form p = case label p of
				"WH" -> transWH (Just p)
				_ -> transTXT (Just p)
			answer p = case label p of
				"WH" -> ( nub . concat ) $ map (
					toupper . named ) $ evalW $ form p
				"YN" -> yesorno $ eval $ form p
				_ -> show $ eval $ form p
	putStrLn $ foldl takeCourse "Unparseable" $ map (\(l,f,a)->l)
		labelFormAnswers
	putStrLn $ foldl pickOne "NoAnswer" $ map (\(l,f,a)->a) labelFormAnswers
	hClose stdout
