import Parsing
import Data.Char
import Data.List

characters = sort $ map (\(x:xs) -> (toUpper x) : xs) $ map (phon . head) people_names

otherwords = map (phon . head) $
	object_names ++ class_names ++
--	prons ++ reflexives ++ interrogatives ++
	aux ++ intransitives ++ transitives ++ ditransitives ++
	preps ++ determiners
--	++ conjuncts

sortedwords = unlines $ map (
	\i -> unwords $ [(toUpper i) : ":"] ++
	[ (l:ls) | (l:ls) <- otherwords, i==l ]
	) ['a'..'z']


main = do
	putStrLn $ unwords characters
	putStr sortedwords
