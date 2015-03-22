module Story_Interpretation where 

import Model

objects = [


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
 ( "radio_and_television", "media" )
 , ( "offer_to_buy", "offered_to_buy" )
 , ( "bought", "buy" )
 , ( "want_to_pay", "wanted_to_pay" )
 , ( "sold", "sell" )
 , ( "want_to_sell", "wanted_to_sell" )
 , ( "grew", "grow" )
 , ( "want_to_grow", "wanted_to_grow" )
 , ( "helped", "help" )
 , ( "want_to_help", "wanted_to_help" )
 , ( "made", "make" )
 , ( "founded", "make" )
 , ( "found", "make" )
 , ( "set_up", "make" )
 , ( "decide_to_make", "make" )
 , ( "decided_to_make", "make" )
 , ( "want_to_make", "wanted_to_make" )
 , ( "studied", "study" )
 , ( "want_to_study", "wanted_to_study" )
 , ( "want_to_buy", "wanted_to_buy" )
 , ( "want_to_look", "wanted_to_look" )
 , ( "want_to_have", "wanted_to_have" )
 , ( "paid", "pay" )
 , ( "promoted", "talked" )
 , ( "promote", "talked" )
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

	, ( "offered_to_buy", \args -> case args of 
		[x,y,z] -> predid3 "offered_to_buy" z y x
		[x,y,z,w] -> offered_to_buy_from w z y x )

	, ( "buy", \args -> case args of 
		[x,y,z,w] -> predid4 "buy" w z y x
		[x,y,z] -> (unditrans . predid4) "buy" z y x
		[x,y] -> (forgetful . unditrans . predid4) "buy" y x
		[x] -> (intransit . forgetful . unditrans . predid4) "buy" x )

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
