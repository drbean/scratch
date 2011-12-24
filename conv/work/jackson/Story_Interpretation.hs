module Story_Interpretation where 

import Model

story_objects = [
	( "rent",	\[x]	-> rent x	),
	( "apartment",	\[x]	-> apartment x	),
	( "unemployment",	\[x]	-> unemployment x	),
	( "administrative_assistant",	\[x]	-> admin x	),
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
 ( "looked", "look" ),
 ( "wear", "wore" ),
 ( "visited", "visit" ),
 ( "interviewed", "interview" ),
 ( "interviewees", "interviewee" ),
 ( "taught", "teach" ),
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

	( "dress",	\[x]	-> officewear x	),

	( "look",	\[x,y] -> look_at y x	),
	( "lose",	\[x,y]	-> lose y x	),

	( "wore",	\[x,y]	-> wore y x	),
	( "interview",	\[x,y]	-> interview y x	)
	]

