module Topic_Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	--( "worker",	\ [x] -> worker x	)
	( "manager",	\[x]	-> predid1 "manager" x	)
	, ( "role",	\[x]	-> predid1 "role" x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "knew", "know" ),
 ( "speak", "talk" ),
 ( "job", "role" ),
 ( "lost", "lose" )
 ]

relations = [

	( "know", \[x,y] -> predid2 "know" y x ),
	( "talked",	\args -> case args of 
		[x,y] -> predid2 "talk_with_or_about" y x
		[x,y,z] -> predid3 "talk_with_about" z y x	),
	( "asked", \args -> case args of
	    [x,y] -> predid2 "asked" y x
	    [x,y,z] -> predid3 "ask_about" z y x ),
	( "said", \[x,y] -> predid2 "said" y x ),
	( "told", \args -> case args of
	    [x,y] -> predid2 "recite" y x
	    [x,y,z] -> predid3 "told" z y x )
 
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
