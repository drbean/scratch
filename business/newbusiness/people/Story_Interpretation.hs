module Story_Interpretation where 

import Model

objects :: [(String, [Entity] -> Bool)]
objects = [
	( "teacher",	\[x]	-> predid1 "teacher" x )
	, ( "ceo",	\[x]	-> predid1 "ceo" x	)
	, ( "company",	\[x]	-> predid1 "company" x	)
	
	]

inflections :: [(String, String)]
inflections = [
	("ceos",	"ceo" )
	, ("companies",	"company") 
	, ("liked",	"like") 
	, ("start",	"started") 

 ]

relations :: [(String, [Entity] -> Bool)]
relations = [
	( "useful",	\[x]	-> predid1 "useful" x )
	, ( "old",	\[x]	-> predid1 "old" x	)
	, ( "true",	\[x] -> predid1 "true" x	)
	, ( "false",	\[x] -> predid1 "false" x	)
	, ( "good",	\[x] -> predid1 "good" x	)
	, ( "bad",	\[x] -> predid1 "bad" x	)
	, ( "successful",	\[x] -> predid1 "successful" x	)
	, ( "unsuccessful",	\[x] -> predid1 "unsuccessful" x	)
	, ( "like",	\[x,y]	-> predid2 "like" y x	)
	, ( "born", \args -> case args of 
		[x,y,z,w] -> predid4 "born" w z y x
		[x,y,z] -> (forgetful4 . predid4) "born" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "born" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "born" x )
	, ( "started", \args -> case args of 
		[x,y,z,w] -> predid4 "started" w z y x
		[x,y,z] -> (forgetful4 . predid4) "started" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "started" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "started" x )

	]

