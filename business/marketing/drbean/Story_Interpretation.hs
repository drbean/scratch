module Story_Interpretation where 

import Model

objects :: [(String, [Entity] -> Bool)]
objects = [
	 ( "good_idea",	\[x] -> predid1 "good_idea" x	)
	 , ( "a_good_price",	\[x] -> predid1 "a_good_price" x	)
	, ( "teacher",	\[x]	-> predid1 "teacher" x )

	]

inflections :: [(String, String)]
inflections = [
 ( "offer_to_buy", "offered_to_buy" )
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

 ]

relations :: [(String, [Entity] -> Bool)]
relations = [
	( "useful",	\[x]	-> predid1 "useful" x )
	, ( "true",	\[x] -> predid1 "true" x	)
	, ( "false",	\[x] -> predid1 "false" x	)
	, ( "successful",	\[x] -> predid1 "successful" x	)
	, ( "unsuccessful",	\[x] -> predid1 "unsuccessful" x	)
	, ( "finish",	\[x,y,z] -> predid3 "finish" z y x	)
	, ( "pay",	\args -> case args of
		[x,y,z,w] -> predid5 "pay" w z y x
		[x,y,z] -> (forgetful5 . predid5) "pay" z y x
		[x,y] -> (forgetful4 . forgetful5 . predid5) "pay" y x
		[x] -> (forgetful3 . forgetful4 . forgetful5) "pay" x )
	, ( "have_to_pay",	\args -> case args of
		[x,y,z] -> predid3 "have_to_pay" z y x )

	, ( "offered_to_buy", \args -> case args of 
		[x,y,z] -> predid3 "offered_to_buy" z y x
		[x,y,z,w] -> offered_to_buy_from w z y x )

	, ( "buy", \args -> case args of 
		[x,y,z,w] -> predid4 "buy" w z y x
		[x,y,z] -> (forgetful4 . predid4) "buy" z y x
		[x,y] -> (forgetful3 .forgetful4 . predid4) "buy" y x
		[x] -> (forgetful2 . forgetful3 . forgetful4 . predid4) "buy" x )

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

	, ( "caused_stress",	\args -> case args of
		[x,y] -> predid2 "cause_stress" y x
		[x] -> (forgetful2 . predid2) "cause_stress" x )
	, ( "felt_stress",	\args -> case args of
		[x,y] -> predid2 "felt_stress" y x
		[x] -> (forgetful2 . predid2) "felt_stress" x )
	, ( "put_pressure",	\[x,y]	-> predid2 "pressurize" y x	)
	, ( "sailed",	\args -> case args of
		[x,y,z,w,v] -> predid5 "sailed" v w z y x
		[x,y,z,w] -> (forgetful5 . predid5 ) "sailed" w z y x
		[x,y,z] -> (forgetful4 . forgetful5 . predid5 ) "sailed" z y x
		[x,y] -> (forgetful3 . forgetful4 . forgetful5 . predid5 ) "sailed" y x )
	, ( "flew",	\args -> case args of
		[x,y,z,w,v] -> predid5 "flew" v w z y x
		[x,y,z,w] -> (forgetful5 . predid5 ) "flew" w z y x
		[x,y,z] -> (forgetful4 . forgetful5 . predid5 ) "flew" z y x
		[x,y] -> (forgetful3 . forgetful4 . forgetful5 . predid5 ) "flew" y x )
	]

