module Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "man",	\ [x] -> isMan x	),
	( "woman",	\ [x] -> isWoman x	),
	( "boy",	\ [x] -> boy x	),
	( "girl",	\ [x] -> isGirl x	),
	( "person",	\ [x] -> people x	),
	( "thing",	\ [x] -> things x	)
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
 ( "talk",	"talked" ),
 ( "say",	"said" ),
 ( "leave",	"left" ),
 ( "give",	"gave" ),
 ( "get",	"got" ),
 ( "bought", "got" ),
 ( "worked",   "work" ),
 
 ( "accepted", "got" ),
 ( "buy",	"bought" ),
 ( "accept",	"accepted" ),
 ( "tell",	"told" ),
 ( "study",    "studied" ),
 ( "go",	"went" ),
 ( "come",	"came" )
 ]

relations = [
 
 ( "had", \[x,y] -> have y x ),
 ( "gave",	\ [x,y,z] ->	gave z y x ),
 ( "work", \args -> case args of
        [x] -> worker x
        [x,y] -> work_where y x || work_as y x ),
 ( "got", \args -> case args of
 	[x,y,z] -> got_from z y x
 	[x,y] -> got y x
	)
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
