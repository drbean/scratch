module Story_Interpretation where 

import Model

objects = [
	( "boss",	\[x] -> boss x	),
	( "company",	\[x] -> company x	),
	( "candidate",	\[x] -> candidate x	),
	( "co-worker",	\[x] -> co_worker x	),
	( "subject",	\[x] -> subject x	),


	( "job",	\[x] -> job x	)
	]

inflections = [
 ( "supervisor", "boss" ),
 ( "co-workers", "co-worker" ),
 ( "information_technology", "i_t" ),
 ( "comments", "comment" ),
 ( "requests", "request" ),
 ( "names", "name" ),
 -- ( "appeared", "appear" ),
 ( "helped", "help" ),
 ( "volunteered", "volunteer" ),
 ( "interviewed", "interview" ),
 ( "interviewees", "interviewee" ),
 ( "taught", "teach" ),
 ( "subjects", "subject" ),
 ( "greeted", "greet" )
 ]

relations = [
	( "thirty",	\[x] -> thirty x	),
	( "fifty-two",	\[x] -> fifty_two x	),
	( "forty-two",	\[x] -> forty_two x	),

	( "energetic",	\[x] -> energetic x	),
	( "confident",	\[x] -> confident x	),
	( "aggressive",	\[x] -> aggressive x	),

	( "calm",	\[x] -> calm x	),
	( "relaxed",	\[x] -> relaxed x	),
	( "hard-working",	\[x] -> hard_working x	),
	( "practical",	\[x] -> practical x	),
	( "reliable",	\[x] -> reliable x	),

	( "quiet",	\[x] -> quiet x	),
	( "nervous",	\[x] -> nervous x	),

	-- ( "appear",	\[x,y] -> appear y x	),
	( "help",	\[x,y]	-> help y x	),

	( "volunteer",	\[x]	-> volunteer x	),
	( "volunteer",	\[x,y]	-> volunteer_at y x	),
	( "interview",	\[x,y]	-> interview y x	),
	( "teach",	\args -> case args of
		[x,y] -> (teach_who y x || teach_what y x)
		[x,y,z] -> teach z y x ),
	( "greet",	\[x,y]	-> greet y x	)
	]

