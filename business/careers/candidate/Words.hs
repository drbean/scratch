import Parsing
import Data.Char
import Data.List

characters = sort $ map (\(x:xs) -> (toUpper x) : xs) people_names

otherwords = pronouns ++ reflexives ++ interrogatives ++ object_names ++ transitives ++ determiners ++ class_names ++ aux ++ ditransitives ++ preps

sortedwords = unlines $ map (
	\i -> unwords $ [(toUpper i) : ":"] ++
	[ (l:ls) | (l:ls) <- otherwords, i==l ]
	) ['a'..'z']


main = do
	putStrLn $ unwords characters
	putStr sortedwords
