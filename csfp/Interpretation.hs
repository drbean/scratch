module Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "person",	\ [x] -> person x	)
	, ( "thing",	\ [x] -> thing x	)
	, ( "man",	\ [x] -> predid1 "male" x	)
	, ( "woman",	\ [x] -> predid1 "female" x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "men",	"man" ),
 ( "women",	"woman" ),
 ( "persons",	"person" ),
 ( "people",	"person" ),
 ( "things",	"thing" ),
 ( "have",	"had" ),
 ( "ask",	"asked" ),
 ( "say",	"said" ),
 ( "leave",	"left" ),
 ( "give",	"gave" ),
 ( "get",	"got" ),
 ( "take",	"got" ),
 ( "took",	"got" ),
 ( "bought", "got" ),
 ( "worked",   "work" ),
 ( "want_to_work", "wanted_to_work" ),
 
 ( "accepted", "got" ),
 ( "buy",	"bought" ),
 ( "accept",	"accepted" ),
 ( "tell",	"told" ),
 ( "study",    "studied" ),
 ( "go",	"went" ),
 ( "come",	"came" )
 ]

relations = [

 ( "true", \[x] -> predid1 "true" x )
 , ( "had", \[x,y] -> predid2 "have" y x )
 , ( "gave",	\ [x,y,z] ->	predid3 "gave" z y x )
 , ( "work", \args -> case args of
        [x] -> predid1 "worker" x
        [x,y] -> predid2 "work" y x )
 , ( "wanted_to_work", \args -> case args of 
 	[x,y,z] -> predid3 "wanted_to_work" z y x
 	[x,y]	-> forgetful3 ( predid3 "wanted_to_work" ) y x )
 , ( "got", \args -> case args of
 	[x,y,z] -> predid3 "got" z y x
 	[x,y] -> forgetful3 (predid3 "got") y x )
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
