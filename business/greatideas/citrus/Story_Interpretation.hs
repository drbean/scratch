module Story_Interpretation where 

import Model

objects = [
	( "oranges",	\[x] -> oranges x	)
	, ( "citrus_fruit",	\[x] -> citrus_fruit x	)
	 ,( "land",	\[x] -> land x	)
	 , ( "experiment",	\[x] -> experiment x	)
	 , ( "good_idea",	\[x] -> good_idea x	)


	]

inflections = [
 ( "offer_to_buy", "offered_to_buy" ),
 ( "bought", "buy" ),
 ( "want_to_pay", "wanted_to_pay" ),
 ( "want_to_sell", "wanted_to_sell" ),
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
		[x,y,z] -> pay z y x
		[x,y] -> forgetful  pay y x
		[x] -> (intransit . forgetful)  pay x )
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
	, ( "wanted_to_sell", \args -> case args of 
		[x,y,z] -> wanted_to_sell z y x
		[w,x,y,z] -> wanted_to_sell_to w z y x )
	, ( "wanted_to_buy", \args -> case args of 
		[x] -> (intransit . forgetful . unditrans) wanted_to_buy x
		[x,y] -> (forgetful . unditrans) wanted_to_buy y x
		[x,y,z] -> unditrans wanted_to_buy z y x
		[x,y,z,w] -> wanted_to_buy w z y x )

	]

