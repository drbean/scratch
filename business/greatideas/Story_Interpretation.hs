module Story_Interpretation where 

import Model

objects = [
	-- ( "polish",	\[x] -> polish x	),
	-- ( "german",	\[x] -> german x	),
	-- ( "american",	\[x] -> american x	),
	-- ( "boss",	\[x] -> boss x	),
	-- ( "company",	\[x] -> company x	),
	-- ( "interviewer",	\[x] -> interviewer x	),
	-- ( "interviewee",	\[x] -> interviewee x	),
	-- ( "candidate",	\[x] -> candidate x	),
	-- ( "co-worker",	\[x] -> co_worker x	),
	-- ( "team_member",	\[x] -> team_member x	),
	-- ( "sales_representative",	\[x] -> sales_representative x	),
	-- ( "sales_experience",	\[x] -> sales_experience x	),
	-- ( "subject",	\[x] -> subject x	),
	-- ( "personality",	\[x] -> personality x	),
	-- ( "ideas",	\[x] -> ideas x	),
	-- ( "presentation",	\[x] -> story x	),
	( "website",	\[x] -> website x	)


	]

inflections = [
 ( "decide_to_charge", "decided_to_charge" ),
 ( "want_to_have", "wanted_to_have" ),
 ( "want_to_finish", "wanted_to_finish" ),
 ( "want_to_pay", "wanted_to_pay" ),
 ( "want_to_pay_sum", "wanted_to_pay_sum" ),
 ( "want", "wanted" )
 ]

relations = [
	( "finish",	\[x,y,z] -> finish z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z] -> pay z y x
		[x,y] -> forgetful  pay y x )
	, ( "offered_to_pay", \[x,y,z] -> offered_to_pay z y x )

	, ( "decided_to_charge",	\args -> case args of
		[x,y,z] -> decided_to_charge z y x
		[x,y,z,w] -> decided_to_charge_entry w z y x	)

	-- -- ( "appear",	\[x,y] -> appear y x	),
	-- ( "help",	\[x,y]	-> help y x	),

	-- ( "became",	\[x,y]	-> becoming y x	),
	-- ( "volunteer",	\[x]	-> volunteer x	),
	-- ( "volunteer",	\[x,y]	-> volunteer_at y x	),
	-- ( "interview",	\[x,y]	-> interview y x	),
	-- ( "teach",	\args -> case args of
	-- 	[x,y] -> (teach_who y x || teach_what y x)
	-- 	[x,y,z] -> teach z y x ),
	-- ( "greet",	\[x,y]	-> greet y x	)
	]

