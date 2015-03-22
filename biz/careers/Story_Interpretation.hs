module Story_Interpretation where 

import Model

objects = [
	( "polish",	\[x] -> polish x	),
	( "german",	\[x] -> german x	),
	( "american",	\[x] -> american x	),
	( "boss",	\[x] -> boss x	),
	( "company",	\[x] -> company x	),
	( "interviewer",	\[x] -> interviewer x	),
	( "interviewee",	\[x] -> interviewee x	),
	( "candidate",	\[x] -> candidate x	),
	( "co-worker",	\[x] -> co_worker x	),
	( "team_member",	\[x] -> team_member x	),
	( "sales_representative",	\[x] -> sales_representative x	),
	( "sales_manager",	\[x] -> sales_manager x	),
	( "sales_experience",	\[x] -> sales_experience x	),
	( "subject",	\[x] -> subject x	),
	( "personality",	\[x] -> personality x	),
	( "ideas",	\[x] -> ideas x	),
	( "presentation",	\[x] -> story x	),


	( "job",	\[x] -> job x	)
	]

inflections = [
 ( "thirty_years_old", "thirty" ),
 ( "fifty-two_years_old", "fifty-two" ),
 ( "forty-two_years_old", "forty-two" ),
 ( "supervisor", "boss" ),
 ( "bosses", "boss" ),
 ( "team_player", "team_member" ),
 ( "team-player", "team_member" ),
 ( "jobs", "job" ),
 ( "co-workers", "co-worker" ),
 ( "candidates", "candidate" ),
 ( "information_technology", "i_t" ),
 ( "comments", "comment" ),
 ( "requests", "request" ),
 ( "names", "name" ),
 -- ( "appeared", "appear" ),
 ( "helped", "help" ),
 ( "want", "wanted" ),
 ( "become", "became" ),
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

	( "successful",	\[x] -> successful x	),
	( "good",	\[x] -> good x	),

	( "energetic",	\[x] -> energetic x	),
	( "confident",	\[x] -> confident x	),
	( "aggressive",	\[x] -> aggressive x	),
	( "ambitious",	\[x] -> ambitious x	),
	( "strong",	\[x] -> strong x	),

	( "calm",	\[x] -> calm x	),
	( "relaxed",	\[x] -> relaxed x	),
	( "hard-working",	\[x] -> hard_working x	),
	( "practical",	\[x] -> practical x	),
	( "reliable",	\[x] -> reliable x	),

	( "quiet",	\[x] -> quiet x	),
	( "nervous",	\[x] -> nervous x	),

	( "wanted",	\[x] -> wanted x	),
	-- ( "appear",	\[x,y] -> appear y x	),
	( "help",	\[x,y]	-> help y x	),

	( "became",	\[x,y]	-> becoming y x	),
	( "volunteer",	\[x]	-> volunteer x	),
	( "volunteer",	\[x,y]	-> volunteer_at y x	),
	( "interview",	\[x,y]	-> interview y x	),
	( "teach",	\args -> case args of
		[x,y] -> (teach_who y x || teach_what y x)
		[x,y,z] -> teach z y x ),
	( "greet",	\[x,y]	-> greet y x	)
	]

