module Topic_Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "worker",	\ [x] -> worker x	),
	( "role",	\[x]	-> role x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "job", "role" ),
 ( "lost", "lose" )
 ]

relations = [

 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
