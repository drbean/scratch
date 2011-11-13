import Parsing
import Data.Char
import Data.List

toupper = reverse . upperizer . reverse where
	upperizer (x:[]) = toUpper x : []
	upperizer (x:'_':xs) = (toUpper x) : '_': (upperizer xs)
	upperizer (x:xs) = x : upperizer xs

characters = sort $ map toupper $ map (phon . head) proper_names

otherwords = map (phon . head) $
	object_names ++ class_names ++
--	prons ++ reflexives ++ interrogatives ++
	aux ++ intransitives ++ transitives ++ ditransitives ++
	preps ++ determiners ++ possessives
--	++ conjuncts

sortedwords = unlines $ map (
	\i -> unwords $ [(toUpper i) : ":"] ++
	[ (l:ls) | (l:ls) <- otherwords, i==l ]
	) ['a'..'z']


classifieds = unlines $ 
	map ( \x -> fst x ++ ":\t" ++ (unwords $ map ( phon . head ) (snd x)) )
	collect_lex

main = do
	putStrLn "Names:"
	putStrLn $ unwords characters
	putStrLn "\nOther words (classified):"
	putStr classifieds
	putStrLn "\nOther words (in alphabetical order):"
	putStr sortedwords
