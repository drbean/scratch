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
	( "website",	\[x] -> website x	),
	( "website_designer",	\[x] -> website_designer x	),
	( "manager",	\[x] -> manager x	)


	]

inflections = [
 ( "want_charge", "wanted_charge" ),
 ( "want_have", "wanted_have" ),
 ( "want_finish", "wanted_finish" ),
 ( "want_pay", "wanted_pay" ),
 ( "want_pay_sum", "wanted_pay_sum" ),
 ( "want", "wanted" )
 ]

relations = [
	-- ( "thirty",	\[x] -> thirty x	),
	-- ( "fifty-two",	\[x] -> fifty_two x	),
	-- ( "forty-two",	\[x] -> forty_two x	),

	-- ( "successful",	\[x] -> successful x	),
	-- ( "good",	\[x] -> good x	),

	-- ( "energetic",	\[x] -> energetic x	),
	-- ( "confident",	\[x] -> confident x	),
	-- ( "aggressive",	\[x] -> aggressive x	),
	-- ( "ambitious",	\[x] -> ambitious x	),
	-- ( "strong",	\[x] -> strong x	),

	-- ( "calm",	\[x] -> calm x	),
	-- ( "relaxed",	\[x] -> relaxed x	),
	-- ( "hard-working",	\[x] -> hard_working x	),
	-- ( "practical",	\[x] -> practical x	),
	-- ( "reliable",	\[x] -> reliable x	),

	-- ( "quiet",	\[x] -> quiet x	),

	( "finish",	\[x,y,z] -> finish z y x	),
	( "pay",	\args -> case args of
		[x,y,z] -> pay z y x
		[x,y] -> forgetful  pay y x ),

	( "wanted_charge",	\args -> case args of
		[x,y,z] -> wanted_charge_sum z y x
		[x,y,z,w] -> wanted_charge w z y x	),
	( "wanted_have",	\[x,y,z] -> wanted_have z y x	),

	( "wanted_finish",	\args -> case args of
		-- [x,y,z] -> wanted_finish z y x
		[x,y,z,w] -> wanted_finish w z y x	),

	-- ( "wanted",	\[x] -> wanted x	),
	( "wanted_pay",	\args -> case args of 
		[x,y,z] -> wanted_pay z y x
		[x,y,z,w] -> wanted_pay_sum w z y x	)

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

