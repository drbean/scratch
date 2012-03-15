module Story_Interpretation where 

import Model

story_objects = [
	( "scoutmaster",	\[x]	-> scoutmaster x	),
	( "assistant_scoutmaster",	\[x]	-> assistant_scoutmaster x	),
	( "eagle_scout",	\[x]	-> eagle_scout x	),
	( "scout",	\[x]	-> scout x	),
	( "leader",	\[x]	-> leader x	),
	( "troop",	\[x]	-> troop x	),

	( "husband",	\[x] -> husband x	),
	( "wife",	\[x] -> wife x	),
	( "parent",	\[x] -> isParent x	),
	( "father",	\[x] -> father x	),
	( "mother",	\[x] -> isMother x	),
	( "daughter",	\[x] -> daughter x	),

	( "traffic_accident",	\[x] -> traffic_accident x	),
	( "brain_damage",	\[x] -> brain_damage x	),

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
 ( "help", "helped" ),
 ( "volunteered", "volunteer" ),
 ( "appreciated", "appreciate" ),
 ( "thanked", "thank" )
 ]

story_relations = [
	( "disabled",	\[x]	-> disabled x	),
	( "mentally-disabled",	\[x]	-> mentally_disabled x	),
	( "dedicated",	\[x]	-> dedicated x	),
	( "physically-disabled",	\[x]	-> physically_disabled x	),
	( "older",	\[x]	-> older x	),

	( "look",	\[x,y] -> look_at y x	),
	( "lose",	\[x,y]	-> lose y x	),
	( "helped",	\[x,y]	-> helped y x	),
	( "appreciate",	\[x,y]	-> appreciate y x	),
	( "thank",	\[x,y]	-> thank y x	),

	( "volunteer",	\[x,y]	-> volunteer_at y x	),

	( "wore",	\[x,y]	-> wore y x	),
	( "interview",	\[x,y]	-> interview y x	)
	]

