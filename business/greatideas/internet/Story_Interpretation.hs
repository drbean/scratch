module Story_Interpretation where 

import Model

objects = [
	( "oranges",	\[x] -> oranges x	)
	, ( "citrus_fruit",	\[x] -> citrus_fruit x	)
	 ,( "land",	\[x] -> land x	)
	 , ( "experiment",	\[x] -> experiment x	)
	 , ( "good_idea",	\[x] -> good_idea x	)
	 , ( "a_good_price",	\[x] -> a_good_price x	)


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
 ( "help_to_make", "helped_to_make" ),
 ( "want_to_buy", "wanted_to_buy" ),
 ( "want_to_look", "wanted_to_look" ),
 ( "want_to_have", "wanted_to_have" ),
 ( "paid", "pay" ),
 ( "want", "wanted" )
 ]

relations = [
	( "successful",	\[x] -> successful x	)
	, ( "unsuccessful",	\[x] -> unsuccessful x	)
	, ( "finish",	\[x,y,z] -> finish z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z,w] -> pay w z y x
		[x,y,z] -> unditrans pay z y x
		[x,y] -> (forgetful . unditrans) pay y x
		[x] -> (intransit . forgetful . unditrans)  pay x )
	, ( "have_to_pay",	\args -> case args of
		[x,y,z] -> have_to_pay z y x )

	, ( "offered_to_buy", \args -> case args of 
		[x,y,z] -> offered_to_buy z y x
		[x,y,z,w] -> offered_to_buy_from w z y x )

	, ( "buy", \args -> case args of 
		[x,y,z] -> buy z y x
		[x,y] -> forgetful  buy y x
		[x] -> (intransit . forgetful)  buy x )

	, ( "wanted_to_pay", \args -> case args of 
		[x,y,z] -> wanted_to_pay z y x )

	, ( "sell", \args -> case args of 
		[x,y] -> forgetful sell y x
		[x,y,z] -> sell z y x )
	, ( "wanted_to_sell", \args -> case args of 
		[x,y,z] -> unditrans wanted_to_sell z y x
		[x,y,z,w] -> wanted_to_sell w z y x )
	, ( "grow", \args -> case args of 
		[x,y] -> grow y x )
	, ( "wanted_to_grow", \args -> case args of 
		[x,y,z] -> wanted_to_grow z y x )
	, ( "help", \args -> case args of 
		[x,y] -> help y x )
	, ( "wanted_to_help", \args -> case args of 
		[x,y,z] -> wanted_to_help z y x )
	, ( "make", \args -> case args of 
		[x,y] -> make y x )
	, ( "helped_to_make", \args -> case args of 
		[x,y,z] -> helped_to_make z y x )
	, ( "wanted_to_buy", \args -> case args of 
		[x] -> (intransit . forgetful . unditrans) wanted_to_buy x
		[x,y] -> (forgetful . unditrans) wanted_to_buy y x
		[x,y,z] -> unditrans wanted_to_buy z y x
		[x,y,z,w] -> wanted_to_buy w z y x )

	]

