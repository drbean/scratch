module Story_Interpretation where 

import Model

story_objects = [
	( "rent",	\[x]	-> rent x	),
	( "home",	\[x]	-> home x	),
	( "unemployment",	\[x]	-> unemployment x	),
	( "officewear",	\[x]	-> officewear x	),
	( "donator",	\[x]	-> donator x	),
	( "ten_dollar_bill",	\[x]	-> bill x	),
	( "black",	\[x]	-> black x	),
	( "white",	\[x]	-> white x	),
	( "birthday_card",	\[x]	-> card x	),
	( "sign",	\[x]	-> sign x	),
	( "shelter",	\[x]	-> shelter x	),
	( "gift",	\[x]	-> gift x	),
	( "smell",	\[x]	-> smell x	),






	( "supervisor",	\[x]	-> supervisor x	),
	( "interviewee",	\[x] -> interviewee x	),


	( "money",	\[x] -> money x	),
	( "story",	\[x] -> story x	),
	( "job",	\[x] -> job x	)
	]

story_inflections = [
 ( "fragrance", "smell" ),
 ( "lotion", "gift" ),
 ( "visitor", "customer" ),
 ( "visitors", "visitor" ),
 ( "names", "name" ),
 ( "cry", "cried" ),
 ( "looked", "look" ),
 ( "wear", "wore" ),
 ( "visited", "visit" ),
 ( "interviewed", "interview" ),
 ( "interviewees", "interviewee" ),
 ( "counselors", "supervisor" ),
 ( "counselor", "supervisor" ),
 ( "supervisors", "supervisor" ),
 ( "taught", "teach" ),
 ( "helped", "help" ),
 ( "lost", "lose" ),
 ( "volunteered", "volunteer" ),
 ( "subjects", "subject" ),
 ( "offended", "offend" ),
 ( "angered", "anger"),
 ( "greeted", "greet" )
 ]

story_relations = [
	( "black",	\[x]	-> black x	),
	( "white",	\[x]	-> white x	),
	( "beautiful",	\[x]	-> beautiful x	),
	( "homeless",	\[x]	-> homeless x	),
	( "older",	\[x]	-> older x	),

	( "dress",	\[x]	-> officewear x	),

	( "cried",	\[x]	-> cried x	),

	( "look",	\[x,y] -> look_at y x	),
	( "lose",	\[x,y]	-> lose y x	),
	( "help",	\[x,y]	-> help y x	),

	( "volunteer",	\[x,y]	-> volunteer_at y x	),

	( "wore",	\[x,y]	-> wore y x	),
	( "interview",	\[x,y]	-> interview y x	)
	]

