module Story_Interpretation where 

import Model

story_objects = [
	( "scoutmaster",	\[x]	-> scoutmaster x	),
	( "assistant_scoutmaster",	\[x]	-> assistant_scoutmaster x	),
	( "leader",	\[x]	-> leader x	),
	( "troop",	\[x]	-> troop x	),

	( "story",	\[x] -> story x	),
	( "job",	\[x] -> job x	)
	]

story_inflections = [
 ( "scouts", "scout" ),
 ( "leaders", "leader" ),
 ( "visitors", "visitor" ),
 ( "names", "name" ),
 ( "cry", "cried" ),
 ( "looked", "look" ),
 ( "helped", "help" ),
 ( "volunteered", "volunteer" ),
 ( "appreciated", "appreciate" ),
 ( "thanked", "thank" )
 ]

story_relations = [
	( "disabled",	\[x]	-> disabled x	),
	( "mentally_disabled",	\[x]	-> mentally_disabled x	),
	( "dedicated",	\[x]	-> dedicated x	),
	( "physically_disabled",	\[x]	-> physically_disabled x	),
	( "older",	\[x]	-> older x	),

	( "look",	\[x,y] -> look_at y x	),
	( "lose",	\[x,y]	-> lose y x	),
	( "help",	\[x,y]	-> help y x	),
	( "appreciate",	\[x,y]	-> appreciate y x	),
	( "thank",	\[x,y]	-> thank y x	),

	( "volunteer",	\[x,y]	-> volunteer_at y x	),

	( "wore",	\[x,y]	-> wore y x	),
	( "interview",	\[x,y]	-> interview y x	)
	]

