import Data.Maybe
import GHC.IO.Handle
import System.IO

import Parsing
import LogicalForm

yesorno :: Answer -> String
yesorno (Boolean False) = "No"
yesorno (Boolean True)  = "Yes"

takeWH :: String -> String -> String
takeWH _ "WH" = "WH"
takeWH "WH" _ = "WH"
takeWH x _    = x

pickOne :: String -> String -> String
pickOne "NoAnswer" x   = x
pickOne "[]" x         = x
pickOne x   "NoAnswer" = x
pickOne x       "[]"   = x
pickOne "False" "True" = "True"
pickOne "True" "False" = "True"
pickOne "False" x      = x
pickOne "True"  x      = x
pickOne y       x      = x ++ ", " ++ y

main = do
	sentence <- getLine
	let lexed = lexer sentence
	putStrLn $ unwords lexed
	let parselist = parses sentence
	hClose stderr
	hDuplicateTo stdout stderr
	let labelFormAnswers =
		map (\p -> (label p,form p,answer p)) parselist where
			label Ep = "Unparseable"
			label p  = catLabel $ t2c p
			form p = case label p of
				"WH" -> transWH $ p
				_ -> transTXT (Just p)
			answer p = case label p of
				"WH" -> show $ map ( toupper . named) $
						evalW $ form p
				"YN" -> yesorno $ eval $ form p
				"S" -> show $ eval $ form p
	putStrLn $ foldl takeWH "S" $ map (\(l,f,a)->l) labelFormAnswers
	putStrLn $ foldl pickOne "NoAnswer" $ map (\(l,f,a)->a) labelFormAnswers
	hClose stdout
