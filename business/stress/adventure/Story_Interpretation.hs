module Story_Interpretation where 

import Model

story_objects = [
	( "adventurer",	\[x]	-> adventurer x	),
	( "teacher",	\[x]	-> teacher x	),

	( "lack_of_control",	\[x] -> lack_of_control x	),
	( "uncertainty",	\[x] -> uncertainty x	),
	( "lack_of_support",	\[x] -> lack_of_support x	),
	( "pressure",	\[x] -> pressure x	),

	( "world",	\[x] -> world x	),
	( "plane",	\[x] -> plane x	),
	( "boat",	\[x] -> boat x	),
	( "glider",	\[x] -> glider x	),
	( "balloon",	\[x] -> balloon x	)

	]

story_inflections = [
 ( "adventurers", "adventurer" ),
 ( "feel_stress", "felt_stress" ),
 ( "sailed", "sail" ),
 ( "flew", "fly" )

 ]

story_relations = [
	( "useful",	\[x]	-> useful x	),

	( "felt_stress",	\args -> case args of [x] -> feel_stress x; [x,y] -> cause_stress y x	),
	( "put_pressure",	\[x,y]	-> pressurize y x	),
	( "sail",	\args -> case args of [x,y] -> sail_around y x; [x,y,z] -> sail_around_in z y x	),
	( "fly",	\args -> case args of [x,y] -> fly_around y x; [x,y,z] -> fly_around_in z y x	),
	( "anger",	\[x,y]	-> anger y x	)
	]

