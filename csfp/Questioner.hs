import Data.Maybe
import GHC.IO.Handle
import System.IO

import Parsing
import LogicalForm

firstparse :: [ParseTree Cat Cat] -> ParseTree Cat Cat
firstparse [] = Ep
firstparse (a:as) = a

yesorno :: Answer -> String
yesorno (Boolean False) = "No"
yesorno (Boolean True)  = "Yes"

main = do
	sentence <- getLine
	let lexed = lexer sentence
	putStrLn $ unwords lexed
	let parse = firstparse. parses $ sentence
	hClose stderr
	hDuplicateTo stdout stderr
	let label = case (parse) of
		Ep -> "Unparseable"
		_  -> catLabel $ t2c parse
	putStrLn $ label
	let answer = case label of
		"WH" -> transWH $ parse
		_ -> transTXT $ Just parse
	let response = case label of
		"WH" -> show $ map ( toupper . named) $ evalW answer
		"YN" -> yesorno $ eval answer
		"S" -> show $ eval answer
	putStrLn response
	hClose stdout
