module Story_Interpretation where 

import Model

objects = [
	-- ( "oranges",	\[x] -> oranges x	)
	-- , ( "website",	\[x] -> website x	)
	 ( "entrance_fee",	\[x] -> entrance_fee x	)
	 , ( "visitors",	\[x] -> visitors x	)
	 , ( "cars",	\[x] -> cars x	)
	 , ( "ten_dollars",	\[x] -> ten_dollars x	)
	 , ( "child",	\[x] -> child x	)
	 , ( "experiment",	\[x] -> experiment x	)


	]

inflections = [
 -- ( "site", "website" ),
 ( "visitor",	"visitors" ),
 ( "car", "cars" ),
 ( "entrance_fee", "ten_dollars" ),
 ( "children", "child" ),
 ( "decide_to_charge", "decided_to_charge" ),
 ( "started_to_charge", "decided_to_charge" ),
 ( "start_to_charge",  "decided_to_charge" ),
 ( "offer_to_buy", "offered_to_buy" ),
 ( "bought", "buy" ),
 ( "want_to_pay", "wanted_to_pay" ),
 ( "want_to_buy", "wanted_to_buy" ),
 ( "want_to_look", "wanted_to_look" ),
 ( "want_to_have", "wanted_to_have" ),
 ( "want_to_finish", "wanted_to_finish" ),
 ( "want_to_finish", "wanted_to_finish" ),
 ( "paid", "pay" ),
 ( "entered", "enter" ),
 ( "paid_to_enter", "pay_to_enter" ),
 ( "want_to_pay_sum", "wanted_to_pay_sum" ),
 ( "want", "wanted" )
 ]

relations = [
	( "successful",	\[x] -> successful x	)
	, ( "unsuccessful",	\[x] -> unsuccessful x	)
	, ( "finish",	\[x,y,z] -> finish z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z] -> pay z y x
		[x,y] -> forgetful  pay y x
		[x] -> (intransit . forgetful)  pay x )
	, ( "enter",	\args -> case args of
		[x,y] -> enter y x )
	, ( "pay_to_enter",	\args -> case args of
		[x,y,z] -> pay_to_enter z y x )
	, ( "have_to_pay",	\args -> case args of
		[x,y,z] -> have_to_pay z y x )

	--, ( "offered_to_buy", \args -> case args of 
	--	[x,y,z] -> offered_to_buy z y x
	--	[x,y,z,w] -> offered_to_buy_from w z y x )
	--, ( "offered_to_pay", \[x,y,z] -> offered_to_pay z y x )

	, ( "buy", \args -> case args of 
		[x,y] -> buy y x )

	, ( "wanted_to_look", \args -> case args of 
		[x,y] -> wanted_to_look y x
		[x,y,z] -> wanted_to_look_at z y x )
	, ( "wanted_to_pay", \args -> case args of 
		[x,y,z] -> wanted_to_pay z y x )
	, ( "wanted_to_buy", \args -> case args of 
		[x,y] -> wanted_to_buy y x
		[x,y,z] -> wanted_to_buy_some z y x )
	, ( "decided_to_charge",	\args -> case args of
		[x,y,z] -> decided_to_charge z y x
		[x,y,z,w] -> decided_to_charge_fee w z y x
		[x,y,z,w,u] -> decided_to_charge_entry u w z y x	)

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

