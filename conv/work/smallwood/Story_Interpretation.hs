module Story_Interpretation where 

import Model

story_objects = [
	( "boss",	\[x] -> boss x	),
	( "company",	\[x] -> company x	),
	( "interviewee",	\[x] -> interviewee x	),
	( "visitor",	\[x] -> visitor x	),
	( "school",	\[x] -> school x	),
	( "subject",	\[x] -> subject x	),
	( "language",	\[x] -> language x	),
	( "request",	\[x] -> request x	),
	( "treatment",	\[x] -> treatment x	),
	( "upbringing",	\[x] -> upbringing x	),


	( "money",	\[x] -> money x	),
	( "story",	\[x] -> story x	),
	( "dress",	\[x] -> dress x	),
	( "job",	\[x] -> job x	)
	]

story_inflections = [
 ( "supervisor", "boss" ),
 ( "information_technology", "i_t" ),
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
	( "red",	\[x]	-> red x	),
	( "offensive",	\[x]	-> offensive x	),
	( "mean",	\[x]	-> mean x	),
	( "unfair",	\[x]	-> unfair x	),

	( "look",	\[x,y] -> look_at y x	),
	( "wore",	\[x,y]	-> wore y x	),
	( "visit",	\[x,y]	-> visit y x	),
	( "interview",	\[x,y]	-> interview y x	),
	( "teach",	\args -> case args of
		[x,y] -> (teach_who y x || teach_what y x)
		[x,y,z] -> teach z y x ),
	( "offend",	\[x,y]	-> offend y x	),
	( "anger",	\[x,y]	-> anger y x	),
	( "greet",	\[x,y]	-> greet y x	)
	]

