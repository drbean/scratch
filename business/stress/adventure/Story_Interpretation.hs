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
 , ( "feel_stress", "felt_stress" )
 , ( "sail", "sailed" )
 , ( "fly", "flew" )
 , ( "flew", "fly" )
 , ( "powered_aircraft", "plane" )

 ]

relations = [
	( "useful",	\[x]	-> predid1 "useful" x )

	-- ( "felt_stress",	\args -> case args of
		-- [x,y] -> predid2 "cause_stress" y x ),
		-- [x] -> predid1 "feel_stress" x ),
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

