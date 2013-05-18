module Story_Interpretation where 

import Model

objects :: [(String, [Entity] -> Bool)]
objects = [
	( "good_idea",	\[x] -> predid1 "good_idea" x	)
	, ( "a_good_price",	\[x] -> predid1 "a_good_price" x	)
	, ( "teacher",	\[x]	-> predid1 "teacher" x )
	, ( "shoes",	\[x]	-> predid1 "shoes" x	)
	, ( "jogging_shoes",	\[x]	-> predid1 "jogging_shoes" x	)
	, ( "slippers",	\[x]	-> predid1 "slippers" x	)
	, ( "men's_formal_shoes",	\[x]	-> predid1 "men's_formal_shoes" x	)
	, ( "women's_formal_shoes",	\[x]	-> predid1 "women's_formal_shoes" x	)
	, ( "rice",	\[x]	-> predid1 "rice" x	)
	, ( "shoe_store",	\[x]	-> predid1 "shoe_store" x	)

	, ( "money",	\[x]	-> predid1 "money" x	)

	]

inflections :: [(String, String)]
inflections = [
 ( "offer_to_ask", "offered_to_ask" )
 , ( "went", "go" )
 , ( "liked", "like" )
 , ( "bought", "ask" )
 , ( "want_to_answer", "wanted_to_answer" )
 , ( "sold", "sell" )
 , ( "want_to_sell", "wanted_to_sell" )
 , ( "want_to_ask", "wanted_to_ask" )
 , ( "want_to_look", "wanted_to_look" )
 , ( "want_to_have", "wanted_to_have" )
 , ( "answered", "answer" )

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
	, ( "answer",	\args -> case args of
		[x,y,z,w] -> predid4 "answer" w z y x
		[x,y,z] -> (forgetful4 . predid4) "answer" z y x
		[x,y] -> (forgetful3 . forgetful4 . predid4) "answer" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "answer" x )
	, ( "have_to_answer",	\args -> case args of
		[x,y,z] -> predid3 "have_to_answer" z y x )

	, ( "ask", \args -> case args of 
		[x,y,z,w] -> predid4 "ask" w z y x
		[x,y,z] -> (forgetful4 . predid4) "ask" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "ask" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "ask" x )
	, ( "have_to_ask",	\args -> case args of
		[x,y,z] -> predid3 "have_to_ask" z y x )

	, ( "wanted_to_answer", \args -> case args of 
		[x,y,z] -> predid3 "wanted_to_answer" z y x )

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
	, ( "wanted_to_ask", \args -> case args of 
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "wanted_to_ask" x
		[x,y] -> (forgetful3 . forgetful4 . predid4) "wanted_to_ask" y x
		[x,y,z] -> (forgetful4 . predid4) "wanted_to_ask" z y x
		[x,y,z,w] -> predid4 "wanted_to_ask" w z y x )

	]

