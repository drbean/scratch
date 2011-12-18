module Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "man",	\ [x] -> isMan x	),
	( "boy",	\ [x] -> boy x	),
	( "woman",	\ [x] -> isWoman x	),
	( "girl",	\ [x] -> isGirl x	),
	( "person",	\ [x] -> people x	),
	( "thing",	\ [x] -> things x	),
	( "parent",	\ [x] -> isParent x	),
	( "mother",	\ [x] -> isMother x	),
	( "father",	\ [x] -> father x	),
	( "daughter",	\ [x] -> daughter x	),
	( "son",	\ [x] -> son x	),
	( "brother",	\ [x] -> brother x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "men",	"man" ),
 ( "boys",	"boy" ),
 ( "women",	"woman" ),
 ( "girls",	"girl" ),
 ( "persons",	"person" ),
 ( "things",	"thing" ),
 ( "parents",	"parent" ),
 ( "fathers",	"father" ),
 ( "mothers",	"mother" ),
 ( "daughters",	"daughter" ),
 ( "sons",	"son" ),
 ( "brothers",	"brother" ),
 ( "sisters",	"sister" ),
 ( "raise",	"raised" ),
 ( "marry",	"married" ),
 ( "got_married",	"get_married" ),
 ( "appreciate",	"appreciated" ),
 ( "disappoint",	"disappointed" ),
 ( "worked",	"work" ),
 ( "have",	"had" ),
 ( "know",	"knew" ),
 ( "looked_back",	"look_back" ),
 ( "ask",	"asked" ),
 ( "talk",	"talked" ),
 ( "say",	"said" ),
 ( "leave",	"left" ),
 ( "immigrated",	"immigrate" ),
 ( "give",	"gave" ),
 ( "get",	"got" ),
 ( "bought", "got" ),
 
 ( "accepted", "got" ),
 ( "buy",	"bought" ),
 ( "accept",	"accepted" ),
 ( "tell",	"told" ),
 ( "study",	"studied" ),
 ( "go",	"went" ),
 ( "come",	"came" )
 ]

relations = [
 ( "raised", \[x,y] -> parented y x ),
 ( "married",	\args -> case args of
 	[x,y] -> married y x
 	[x,y,z] -> marry_in z y x ),
 ( "get_married", \args -> case args of
 	[x] -> isMarried x
 	[x,y] -> wedded_in y x ),
 
 ( "appreciated", \[x,y] -> appreciate y x ),
 ( "work", \args -> case args of
 	[x] -> worker x || job x
 	[x,y] -> work_where y x || work_as y x ),
 ( "had", \[x,y] -> have y x ),
 ( "knew", \[x,y] -> know y x ),
 ( "look_back", \args -> case args of
 	[x] -> look_back x
 	[x,y] -> look_back_on y x ),
 ( "asked", \args -> case args of
 	[x,y] -> asked y x
 	[x,y,z] -> ask_about z y x ),
 ( "talked", \args -> case args of 
 	[x,y] -> talked y x
 	[x,y,z] -> talk_about z y x ),
 ( "said", \[x,y] -> said y x ),
 
 ( "gave",	\ [x,y,z] ->	gave z y x ),
 ( "got", \args -> case args of
 	[x,y] -> forgetful got y x
 	[x,y,z] -> got z y x ),
 ( "told", \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x ),
 ( "studied", \args -> case args of
 	[x,y] -> (studied_where y x || studied_what y x)
 	[x,y,z] -> studied z y x )
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
