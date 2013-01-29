module Story_Interpretation where 

import Model

objects = [
	( "adventurer",	\[x]	-> predid1 "adventurer" ),
	( "teacher",	\[x]	-> predid1 "teacher" ),

	( "lack_of_control",	\[x] -> predid1 "lack_of_control" ),
	( "uncertainty",	\[x] -> predid1 "uncertainty" ),
	( "lack_of_support",	\[x] -> predid1 "lack_of_support" ),
	( "pressure",	\[x] -> predid1 "pressure" ),

	( "world",	\[x] -> predid1 "world" ),
	( "plane",	\[x] -> predid1 "plane" ),
	( "boat",	\[x] -> predid1 "boat" ),
	( "glider",	\[x] -> predid1 "glider" ),
	( "balloon",	\[x] -> predid1 "balloon" )

	]

inflections = [
 ( "adventurers", "adventurer" ),
 ( "feel_stress", "felt_stress" ),
 ( "sailed", "sail" ),
 ( "flew", "fly" )

 ]

relations = [
	( "useful",	\[x]	-> predid1 "useful" ),

	( "felt_stress",	\args -> case args of
		[x,y] -> predid2 "cause_stress" y x ),
		-- [x] -> predid1 "feel_stress" x ),
	-- ( "put_pressure",	\[x,y]	-> pressurize y x	),
	( "sail",	\args -> case args of [x,y] -> sail_around y x; [x,y,z] -> sail_around_in z y x	),
	( "fly",	\args -> case args of [x,y] -> fly_around y x; [x,y,z] -> fly_around_in z y x	),
	( "anger",	\[x,y]	-> anger y x	)
	]

