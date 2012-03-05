module Story_Interpretation where 

import Model

story_objects = [
	( "adventurer",	\[x]	-> adventurer x	),
	( "teacher",	\[x]	-> teacher x	),

	-- ( "control",	\[x] -> control x	),
	-- ( "uncertainty",	\[x] -> uncertainty x	),
	-- ( "support",	\[x] -> support x	),
	( "pressure",	\[x] -> pressure x	)

	]

story_inflections = [
 ( "adventurers", "adventurer" ),
 ( "frameworks", "framework" )

 ]

story_relations = [
	( "useful",	\[x]	-> useful x	),

	( "put_pressure",	\[x,y]	-> pressurize y x	),
	( "sail",	\[x,y]	-> sail y x	),
	( "anger",	\[x,y]	-> anger y x	)
	]

