module Story_Interpretation where 

import Model

objects :: [(String, [Entity] -> Bool)]
objects = [
	( "teacher",	\[x]	-> predid1 "teacher" x )
	, ( "student",	\[x]	-> predid1 "student" x	)
	, ( "company",	\[x]	-> predid1 "company" x	)
	, ( "master's_degree",	\[x]	-> predid1 "master's_degree" x	)
	, ( "phd_degree",	\[x]	-> predid1 "phd_degree" x	)
	, ( "electrical_engineering",	\[x]	-> predid1 "electrical_engineering" x)
	, ( "mechanical_engineering",	\[x]	-> predid1 "mechanical_engineering" x)


	
	]

inflections :: [(String, String)]
inflections = [
	("students",	"student" )
	, ("companies",	"company") 
	, ("liked",	"like") 
	, ("lived",	"live") 
	, ("cats",	"cat") 
	, ("brothers",	"brother") 
	, ("sisters",	"sister") 
	, ("siblings",	"sibling") 

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

	, ( "applied_foreign_languages",	\[x] -> predid1 "applied_foreign_languages" x	)
	, ( "english",	\[x] -> predid1 "english" x	)

	, ( "father",	\[x] -> predid1 "father" x	)
	, ( "mother",	\[x] -> predid1 "mother" x	)
	, ( "brother",	\[x] -> predid1 "brother" x	)
	, ( "sister",	\[x] -> predid1 "sister" x	)
	, ( "sibling",	\[x] -> predid1 "sibling" x	)
	, ( "grandmother",	\[x] -> predid1 "grandmother" x	)

	, ( "career_woman",	\[x] -> predid1 "career_woman" x	)
	, ( "truck_driver",	\[x] -> predid1 "truck_driver" x	)
	, ( "farmer",	\[x] -> predid1 "farmer" x	)

	, ( "cat",	\[x] -> predid1 "cat" x	)

	, ( "design_assistant",	\[x] -> predid1 "design_assistant" x	)

	, ( "24",	\[x] -> predid1 "24" x	)

	, ( "babysitter",	\[x] -> predid1 "babysitter" x	)

	, ( "like",	\[x,y]	-> predid2 "like" y x	)
	, ( "live",	\[x,y]	-> predid2 "live" y x	)
	, ( "kind",	\[x,y]	-> predid2 "kind" y x	)

	, ( "held", \args -> case args of 
		[x,y,z,w] -> predid4 "held" w z y x
		[x,y,z] -> (forgetful4 . predid4) "held" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "held" y x
		)
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

