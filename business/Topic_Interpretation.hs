module Topic_Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "administrative_assistant",   \ [x] -> secretary x    ),
	( "worker",	\ [x] -> worker x	),
	( "job",	\[x]	-> job x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "helped", "help" ),
 ( "lost", "lose" ),
 ( "volunteered", "volunteer" )
 ]

relations = [
	( "help",	\[x,y]	-> help y x	),

	( "volunteer",	\[x]	-> volunteer x	),
	( "volunteer",	\[x,y]	-> volunteer_at y x	)
 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
