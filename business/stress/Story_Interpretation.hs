module Story_Interpretation where 

import Model

story_objects = [
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

	( "supervisor",	\[x]	-> supervisor x	),
	( "subordinate",	\[x] -> subordinate x	),

	-- ( "control",	\[x] -> control x	),
	-- ( "uncertainty",	\[x] -> uncertainty x	),
	-- ( "support",	\[x] -> support x	),
	( "pressure",	\[x] -> pressure x	),

	( "order",	\[x] -> order x	),
	( "goods",	\[x] -> goods x	),
	( "company",	\[x] -> company x	),
	( "framework",	\[x] -> framework x	)
	]

story_inflections = [
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

 ( "orders", "order" ),
 ( "goods", "good" ),
 ( "companies", "company" ),
 ( "frameworks", "framework" )

 ]

story_relations = [
	( "angry",	\[x]	-> angry x	),
	( "brilliant",	\[x]	-> brilliant x	),
	--( "stressful",	\[x]	-> stressful x	),
	( "useful",	\[x]	-> useful x	),

	( "put_pressure",	\[x,y]	-> pressurize y x	),
	( "anger",	\[x,y]	-> anger y x	)
	]

