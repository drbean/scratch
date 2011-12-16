module Story_Interpretation where 

import Model

story_objects = [
	( "boss",	\[x] -> boss x	),
	( "company",	\[x] -> company x	),
	( "receptionist",	\[x] -> receptionist x	),
	( "customer",	\[x] -> customer x	),
	( "hospital",	\[x] -> hospital x	),
	( "company",	\[x] -> company x	),
	( "visitor",	\[x] -> visitor x	),
	( "school",	\[x] -> school x	),

	( "i_t",	\[x] -> i_t x	),

	( "money",	\[x] -> money x	),
	( "story",	\[x] -> story x	),
	( "aunt",	\[x] -> aunt x	),
	( "niece",	\[x] -> niece x	),
	( "job",	\[x] -> job x	)
	]

story_inflections = [
 ( "supervisor", "boss" ),
 ( "information_technology", "i_t" ),
 ( "customers", "customer" ),
 ( "visitor", "customer" ),
 ( "visitors", "visitor" ),
 ( "greeted", "greet" )
 ]

story_relations = [
	( "greet",	\[x,y] -> greet x y	)
	]

