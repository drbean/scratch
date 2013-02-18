module Story_Interpretation where 

import Model

objects = [
	( "adventurer",	\[x]	-> predid1 "adventurer" x )
	, ( "teacher",	\[x]	-> predid1 "teacher" x )
	, ( "team",	\[x]	-> predid1 "team" x )

	, ( "lack_of_control",	\[x] -> predid1 "lack_of_control" x )
	, ( "uncertainty",	\[x] -> predid1 "uncertainty" x )
	, ( "lack_of_support",	\[x] -> predid1 "lack_of_support" x )
	, ( "pressure",	\[x] -> predid1 "pressure" x )
	, ( "stress",	\[x] -> predid1 "stress" x )
	, ( "stressful",	\[x] -> predid1 "stressful" x )

	, ( "world",	\[x] -> predid1 "world" x )
	, ( "plane",	\[x] -> predid1 "plane" x )
	, ( "boat",	\[x] -> predid1 "boat" x )
	, ( "glider",	\[x] -> predid1 "glider" x )
	, ( "balloon",	\[x] -> predid1 "balloon" x )
	, ( "aircraft",	\[x] -> predid1 "aircraft" x )

	]

inflections = [
 ( "adventurers", "adventurer" )
 , ( "cause_stress", "caused_stress" )
 , ( "feel_stress", "felt_stress" )
 , ( "sail", "sailed" )
 , ( "fly", "flew" )
 , ( "powered_aircraft", "plane" )

 ]

relations = [
	( "useful",	\[x]	-> predid1 "useful" x )
	, ( "ambitious",	\[x]	-> predid1 "ambitious" x )

	, ( "caused_stress",	\args -> case args of
		[x,y] -> predid2 "cause_stress" x y
		[x] -> (forgetful2 . predid2) "cause_stress" x )
	, ( "felt_stress",	\args -> case args of
		[x,y] -> predid2 "felt_stress" x y
		[x] -> (forgetful2 . predid2) "felt_stress" x )
	, ( "put_pressure",	\[x,y]	-> predid2 "pressurize" y x	)
	, ( "sailed",	\args -> case args of
		[x,y,z,w,v] -> predid5 "sailed" v w z y x
		[x,y,z,w] -> (forgetful5 . predid5 ) "sailed" w z y x
		[x,y,z] -> (forgetful4 . forgetful5 . predid5 ) "sailed" x y z
		[x,y] -> (forgetful3 . forgetful4 . forgetful5 . predid5 ) "sailed" x y )
	, ( "flew",	\args -> case args of
		[x,y,z,w,v] -> predid5 "flew" v w z y x
		[x,y,z,w] -> (forgetful5 . predid5 ) "flew" w z y x
		[x,y,z] -> (forgetful4 . forgetful5 . predid5 ) "flew" x y z
		[x,y] -> (forgetful3 . forgetful4 . forgetful5 . predid5 ) "flew" x y )
	]

