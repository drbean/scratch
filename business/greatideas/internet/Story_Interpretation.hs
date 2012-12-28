module Story_Interpretation where 

import Model

objects = [


	 ( "experiment",	\[x] -> predid1 "experiment" x	)
	 , ( "good_idea",	\[x] -> predid1 "good_idea" x	)
	 , ( "a_good_price",	\[x] -> predid1 "a_good_price" x	)
	 , ( "advertisers",	\[x] -> predid1 "advertisers" x	)
	 , ( "advertising_space",	\[x] -> predid1 "advertising_space" x	)


	]

inflections = [
 ( "offer_to_buy", "offered_to_buy" ),
 ( "bought", "buy" ),
 ( "want_to_pay", "wanted_to_pay" ),
 ( "sold", "sell" ),
 ( "want_to_sell", "wanted_to_sell" ),
 ( "grew", "grow" ),
 ( "want_to_grow", "wanted_to_grow" ),
 ( "helped", "help" ),
 ( "want_to_help", "wanted_to_help" ),
 ( "made", "make" ),
 ( "founded", "make" ),
 ( "set_up", "make" ),
 ( "help_to_make", "helped_to_make" ),
 ( "want_to_buy", "wanted_to_buy" ),
 ( "want_to_look", "wanted_to_look" ),
 ( "want_to_have", "wanted_to_have" ),
 ( "paid", "pay" ),
 ( "want", "wanted" )
 ]

relations = [
	( "successful",	\[x] -> predid1 "successful" x	)
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
		[x,y,z] -> predid3 "buy" z y x
		[x,y] -> forgetful (predid3 "buy") y x
		[x] -> (intransit . forgetful) (predid3 "buy") x )

	, ( "wanted_to_pay", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_pay" z y x )

	, ( "sell", \args -> case args of 
		[x,y] -> forgetful (predid3 "sell") y x
		[x,y,z] -> predid3 "sell" z y x )
	, ( "wanted_to_sell", \args -> case args of 
		[x,y,z] -> unditrans wanted_to_sell z y x
		[x,y,z,w] -> wanted_to_sell w z y x )
	, ( "help", \args -> case args of 
		[x,y] -> predid2 "help" y x )
	, ( "wanted_to_help", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_help" z y x )
	, ( "make", \args -> case args of 
		[x,y] -> predid2 "make" y x )
	, ( "helped_to_make", \args -> case args of 
		[x,y,z] -> predid3 "helped_to_make" z y x )
	, ( "wanted_to_buy", \args -> case args of 
		[x] -> (intransit . forgetful . unditrans) wanted_to_buy x
		[x,y] -> (forgetful . unditrans) wanted_to_buy y x
		[x,y,z] -> unditrans wanted_to_buy z y x
		[x,y,z,w] -> wanted_to_buy w z y x )

	]

-- vim: set ts=8 sts=4 sw=4 noet:
