module Topic_Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

topic_objects, topic_relations :: [( String, [Entity] -> Bool)]
topic_objects = [
	( "daughter",	\ [x] -> daughter x	),
	( "mother",	\ [x] -> isMother x	)
 ]

topic_inflections :: [(String, String)]
topic_inflections = [
 ]

topic_relations = [
 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
