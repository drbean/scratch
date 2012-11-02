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
 ( "speak", "talk" ),
 ( "job", "role" ),
 ( "lost", "lose" )
 ]

relations = [

	( "know", \[x,y] -> know y x ),
	( "talk",	\args -> case args of 
		[x,y] -> talk_with_or_about y x
		[x,y,z] -> talk_with_about z y x	),
	( "asked", \args -> case args of
	    [x,y] -> asked y x
	    [x,y,z] -> ask_about z y x ),
	( "said", \[x,y] -> said y x ),
	( "told", \args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x )
 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
