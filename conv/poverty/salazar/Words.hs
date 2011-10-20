import Parsing
import Data.Char
import Data.List

upperizer :: String -> String
upperizer (x:[]) = toUpper x : []
upperizer (x:'_':xs) = (toUpper x) : '_': (upperizer xs)
upperizer (x:xs) = x : upperizer xs

characters = sort $ map ( reverse . upperizer . reverse ) $ map (phon . head) people_names

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
