module Story_Interpretation where 

import Model

objects :: [(String, [Entity] -> Bool)]
objects = [
	 ( "good_idea",	\[x] -> predid1 "good_idea" x	)
	 , ( "a_good_price",	\[x] -> predid1 "a_good_price" x	)
	, ( "teacher",	\[x]	-> predid1 "teacher" x )
	, ( "shoes",	\[x]	-> predid1 "shoes" x	)
	, ( "oil",	\[x]	-> predid1 "oil" x	)
	, ( "milk",	\[x]	-> predid1 "milk" x	)
	, ( "bananas",	\[x]	-> predid1 "bananas" x	)
	, ( "eggs",	\[x]	-> predid1 "eggs" x	)
	, ( "rice",	\[x]	-> predid1 "rice" x	)

	, ( "money",	\[x]	-> predid1 "money" x	)

	]

inflections :: [(String, String)]
inflections = [
 ( "offer_to_buy", "offered_to_buy" )
 , ( "went", "go" )
 , ( "liked", "like" )
 , ( "bought", "buy" )
 , ( "want_to_pay", "wanted_to_pay" )
 , ( "sold", "sell" )
 , ( "want_to_sell", "wanted_to_sell" )
 , ( "want_to_buy", "wanted_to_buy" )
 , ( "want_to_look", "wanted_to_look" )
 , ( "want_to_have", "wanted_to_have" )
 , ( "paid", "pay" )

 ]

relations :: [(String, [Entity] -> Bool)]
relations = [
	( "useful",	\[x]	-> predid1 "useful" x )
	, ( "old",	\[x]	-> predid1 "old" x	)
	, ( "true",	\[x] -> predid1 "true" x	)
	, ( "false",	\[x] -> predid1 "false" x	)
	, ( "successful",	\[x] -> predid1 "successful" x	)
	, ( "unsuccessful",	\[x] -> predid1 "unsuccessful" x	)
	, ( "go",	\[x,y]	-> predid2 "go" y x	)
	, ( "like",	\[x,y]	-> predid2 "like" y x	)
	, ( "finish",	\[x,y,z] -> predid3 "finish" z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z,w] -> predid4 "pay" w z y x
		[x,y,z] -> (forgetful4 . predid4) "pay" z y x
		[x,y] -> (forgetful3 . forgetful4 . predid4) "pay" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "pay" x )
	, ( "have_to_pay",	\args -> case args of
		[x,y,z] -> predid3 "have_to_pay" z y x )

	, ( "buy", \args -> case args of 
		[x,y,z,w] -> predid4 "buy" w z y x
		[x,y,z] -> (forgetful4 . predid4) "buy" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "buy" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "buy" x )
	, ( "have_to_buy",	\args -> case args of
		[x,y,z] -> predid3 "have_to_buy" z y x )

	, ( "wanted_to_pay", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_pay" z y x )

	, ( "sell", \args -> case args of 
		[x,y] -> (forgetful3 . forgetful4 . predid4) "sell" y x
		[x,y,z] -> (forgetful4 . predid4) "sell" z y x
		[x,y,z,w] -> predid4 "sell" w z y x )
	, ( "help", \args -> case args of 
		[x,y] -> predid2 "help" y x )
	, ( "wanted_to_help", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_help" z y x )
	, ( "make", \args -> case args of 
		[x,y] -> (forgetful3 . predid3) "make" y x
		[x,y,z] -> predid3 "make" z y x )
	, ( "wanted_to_make", \args -> case args of 
		[x,y,z,w] -> predid4 "wanted_to_make" w z y x
		[x,y,z] -> (forgetful4 . predid4) "wanted_to_make" z y x)
	, ( "wanted_to_buy", \args -> case args of 
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "wanted_to_buy" x
		[x,y] -> (forgetful3 . forgetful4 . predid4) "wanted_to_buy" y x
		[x,y,z] -> (forgetful4 . predid4) "wanted_to_buy" z y x
		[x,y,z,w] -> predid4 "wanted_to_buy" w z y x )

	]

