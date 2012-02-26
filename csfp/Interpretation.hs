module Interpretation where 

import Data.List

import Model
-- import Story_Interpretation

objects, relations :: [( String, [Entity] -> Bool)]
objects = [
	( "man",	\ [x] -> male x	),
	( "woman",	\ [x] -> female x	),
	( "person",	\ [x] -> people x	),
	( "thing",	\ [x] -> things x	),
	( "supervisor",	\ [x] -> supervisor x	),
	( "subordinate",	\ [x] -> subordinate x	)
 ]

inflections :: [(String, String)]
inflections = [
 ( "men",	"man" ),
 ( "women",	"woman" ),
 ( "persons",	"person" ),
 ( "things",	"thing" ),
 ( "supervisors",	"supervisor" ),
 ( "subordinates",	"subordinate" ),
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
 
 ( "appreciated", \[x,y] -> appreciate y x ),
 ( "work", \args -> case args of
 	[x] -> worker x
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
