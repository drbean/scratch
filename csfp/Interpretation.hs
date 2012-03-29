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
 ( "appreciate",	"appreciated" ),
 ( "disappoint",	"disappointed" ),
 ( "have",	"had" ),
 ( "looked_back",	"look_back" ),
 ( "ask",	"asked" ),
 ( "talk",	"talked" ),
 ( "say",	"said" ),
 ( "leave",	"left" ),
 ( "immigrated",	"immigrate" ),
 ( "give",	"gave" ),
 ( "get",	"got" ),
 ( "bought", "got" ),
 ( "worked",   "work" ),
 
 ( "accepted", "got" ),
 ( "buy",	"bought" ),
 ( "accept",	"accepted" ),
 ( "tell",	"told" ),
 ( "go",	"went" ),
 ( "come",	"came" )
 ]

relations = [
 
 ( "appreciated", \[x,y] -> appreciate y x ),
 ( "had", \[x,y] -> have y x ),
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
 
 ( "talked", \args -> case args of 
 	[x,y] -> talked y x
 	[x,y,z] -> talk_about z y x ),
 ( "gave",	\ [x,y,z] ->	gave z y x ),
 ( "work", \args -> case args of
        [x] -> worker x
        [x,y] -> work_where y x || work_as y x ),
 ( "got", \args -> case args of
 	-- [x,y,z] -> got z y x
 	[x,y] -> got y x
	),
 ( "told", \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x )
 ]

-- vim: set ts=8 sts=4 sw=4 noet:
