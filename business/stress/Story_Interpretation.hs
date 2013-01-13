module Story_Interpretation where 

import Model

objects = [
	( "adventurer",	\[x]	-> adventurer x	),
	( "teacher",	\[x]	-> teacher x	),
	( "psychologist",	\[x]	-> psychologist x	),
	( "doctor",	\[x]	-> doctor x	),
	( "office_worker",	\[x]	-> office_worker x	),
	( "ceo",	\[x]	-> ceo x	),
	( "production_manager",	\[x]	-> production_manager x	),
	( "salesman",	\[x]	-> salesman x	),
	( "sales_manager",	\[x]	-> sales_manager x	),
	( "customer",	\[x]	-> customer x	),
	( "oranges",	\[x] -> oranges x	)
	, ( "citrus_fruit",	\[x] -> citrus_fruit x	)
	 ,( "land",	\[x] -> land x	)
	 , ( "experiment",	\[x] -> experiment x	)
	 , ( "good_idea",	\[x] -> good_idea x	)
	 , ( "a_good_price",	\[x] -> a_good_price x	)
	-- ( "control",	\[x] -> control x	),
	-- ( "uncertainty",	\[x] -> uncertainty x	),
	-- ( "support",	\[x] -> support x	),
	( "pressure",	\[x] -> pressure x	),

	( "order",	\[x] -> order x	),
	( "goods",	\[x] -> goods x	),
	( "company",	\[x] -> company x	),
	( "framework",	\[x] -> framework x	)
	 ( "experiment",	\[x] -> predid1 "experiment" x	)
	 , ( "good_idea",	\[x] -> predid1 "good_idea" x	)
	 , ( "a_good_price",	\[x] -> predid1 "a_good_price" x	)
	 , ( "advertisers",	\[x] -> predid1 "advertisers" x	)
	 , ( "advertising_space",	\[x] -> predid1 "advertising_space" x	)
	 , ( "media",	\[x] -> predid1 "media" x	)
	 , ( "website",	\[x] -> predid1 "website" x	)
	 , ( "business_management",	\[x] -> predid1 "business_management" x	)
	, ( "money",	\[x]	-> predid1 "money" x	)


	]

inflections = [
 ( "adventurers", "adventurer" ),
 ( "teachers", "teacher" ),
 ( "psychologists", "psychologist" ),
 ( "doctors", "doctor" ),
 ( "office_workers", "office_worker" ),
 ( "ceos", "ceo" ),
 ( "production_managers", "production_manager" ),
 ( "salesmen", "salesman" ),
 ( "sales_managers", "sales_manager" ),
 ( "customers", "customer" ),
 , ( "promoted", "talked" )
 , ( "promote", "talked" )
 ( "orders", "order" ),
 ( "goods", "good" ),
 ( "companies", "company" ),
 ( "frameworks", "framework" )
 ]

relations = [
	( "true",	\[x] -> predid1 "true" x	)
	, ( "false",	\[x] -> predid1 "false" x	)
	, ( "successful",	\[x] -> predid1 "successful" x	)
	, ( "unsuccessful",	\[x] -> predid1 "unsuccessful" x	)
	, ( "finish",	\[x,y,z] -> predid3 "finish" z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z,w] -> pay w z y x
		[x,y,z] -> unditrans pay z y x
		[x,y] -> (forgetful . unditrans) pay y x
		[x] -> (intransit . forgetful . unditrans)  pay x )
	, ( "have_to_pay",	\args -> case args of
		[x,y,z] -> predid3 "have_to_pay" z y x )
 ]

	, ( "offered_to_buy", \args -> case args of 
		[x,y,z] -> predid3 "offered_to_buy" z y x
		[x,y,z,w] -> offered_to_buy_from w z y x )
	( "angry",	\[x]	-> angry x	),
	( "brilliant",	\[x]	-> brilliant x	),
	--( "stressful",	\[x]	-> stressful x	),
	( "useful",	\[x]	-> useful x	),
	, ( "buy", \args -> case args of 
		[x,y,z,w] -> predid4 "buy" w z y x
		[x,y,z] -> (unditrans . predid4) "buy" z y x
		[x,y] -> (forgetful . unditrans . predid4) "buy" y x
		[x] -> (intransit . forgetful . unditrans . predid4) "buy" x )

	( "put_pressure",	\[x,y]	-> pressurize y x	),
	( "anger",	\[x,y]	-> anger y x	)
	, ( "wanted_to_pay", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_pay" z y x )

	, ( "sell", \args -> case args of 
		[x,y] -> (forgetful . unditrans . predid4) "sell" y x
		[x,y,z] -> (unditrans . predid4) "sell" z y x
		[x,y,z,w] -> predid4 "sell" w z y x )
	, ( "help", \args -> case args of 
		[x,y] -> predid2 "help" y x )
	, ( "wanted_to_help", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_help" z y x )
	, ( "make", \args -> case args of 
		[x,y] -> forgetful (predid3 "make") y x
		[x,y,z] -> predid3 "make" z y x )
	, ( "wanted_to_make", \args -> case args of 
		[x,y,z,w] -> predid4 "wanted_to_make" w z y x
		[x,y,z] -> (unditrans . predid4) "wanted_to_make" z y x)
	, ( "study", \args -> case args of 
		[x] -> intransit (predid2 "studied") x
		[x,y] -> (predid2 "studied") y x )
	, ( "wanted_to_study", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_study" z y x
		[x,y] -> (forgetful . predid3) "wanted_to_study" y x
		[x] -> (intransit . forgetful . predid3) "wanted_to_study" x)
	, ( "wanted_to_buy", \args -> case args of 
		[x] -> (intransit . forgetful . unditrans) wanted_to_buy x
		[x,y] -> (forgetful . unditrans) wanted_to_buy y x
		[x,y,z] -> unditrans wanted_to_buy z y x
		[x,y,z,w] -> wanted_to_buy w z y x )
	--, ( "decided_to_make",	\args -> case args of
	--	[x,y,z] -> (forgetful . unditrans) decided_to_make z y x
	--	[x,y,z] -> (forgetful . unditrans) decided_to_make z y x
	--	[x,y,z,w] -> unditrans decided_to_make w z y x
	--	[x,y,z,w,u] -> decided_to_make u w z y x	)

	]

-- vim: set ts=8 sts=4 sw=4 noet:
