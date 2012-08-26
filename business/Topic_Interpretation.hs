module Topic_Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "administrative_assistant",   \ [x] -> secretary x    ),
	( "worker",	\ [x] -> worker x	)
 ]

inflections :: [(String, String)]
inflections = [
 ]

relations = [
	( "job",	\[x]	-> job x	)
 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
