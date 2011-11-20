module Interpretation where 

import Model

type Interp a	= String -> [a] -> Bool

int :: Interp Entity
int "man"	= \ [x] -> isMan x
int "men"	= \ [x] -> isMan x
int "boy"	= \ [x] -> boy x; int "boys" = int "boy"
int "woman"	= \ [x] -> isWoman x
int "women"	= \ [x] -> isWoman x
int "girl"	= \ [x] -> isGirl x; int "girls" = int "girl"

int "person"	= \ [x] -> people x
int "thing"	= \ [x]	-> things x

int "parent" = \[x] -> isParent x
int "raised" = \[x,y] -> parented y x;	int "raise" = int "raised"
int "married"	= \args -> case args of
	[x,y] -> married y x
	[x,y,z] -> marry_in z y x
int "marry"	= int "married"
int "get_married" = \args -> case args of
	[x] -> isMarried x
	[x,y] -> wedded_in y x
int "parents" = \ [x] -> isParent x
int "mother" = \ [x] -> isMother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "son" = \ [x] -> son x
int "brother" = \ [x] -> brother x

int "disappointed" = \[x,y] -> disappoint y x
int "disappoint" = int "disappointed"
int "work" = \args -> case args of
	[x] -> worker x
	[x,y] -> work_where y x || work_as y x
int "worked" = int "work"
int "had" = \[x,y] -> have y x;	int "have" = int "had"
int "knew" = \[x,y] -> know y x; int "know" = int "knew"
int "look_back" = \args -> case args of
	[x] -> look_back x
	[x,y] -> look_back_on y x
int "looked_back" = int "look_back"
int "spoke" = \[x,y] -> speak y x; int "speak" = int "spoke"
int "asked" = \args -> case args of 
	[x,y] -> asked y x
	[x,y,z] -> ask_about z y x
int "ask" = int "asked"
int "talked" = \args -> case args of 
	[x,y] -> talked y x
	[x,y,z] -> talk_about z y x
int "talk" = int "talked"
int "said" = \[x,y] -> said y x; int "say" = int "said"
int "left" = \args -> case args of
	[x] -> isImmigrant x
	[x,y] -> come_from y x
	[x,y,z] -> immigrate z y x
int "leave" = int "left"
int "immigrate" = \args -> case args of
	[x] -> isImmigrant x
	[x,y] -> go_to y x
	[x,y,z] -> immigrate z y x


int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "got" = \[x,y,z] -> gave x y z; int "get" = int "got"
int "bought" = int "got"; int "buy" = int "get"

int "accepted" = int "got"; int "accept" = int "accepted"
int "told" = \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x
int "tell" = int "told"
int "studied" = \args -> case args of
	[x,y] -> (studied_where y x || studied_what y x)
	[x,y,z] -> studied z y x
int "study" = int "studied"
int "went" = \[x,y] -> go_to y x; int "go" = int "went"
int "came" = \[x,y] -> come_from y x; int "come" = int "came"

int "culture" = \[x] -> culture x
int "upbringing"	= \[x] -> upbringing x
int "necklace"	= \[x] -> necklace x
int "present"	= \[x] -> present x
int "sense"	= \[x] -> sense x
int "story"	= \[x] -> story x
int "doll" = \ [x] -> doll x;	int "dolls" = int "doll"
int "motel" = \ [x] -> motel x
int "boat" = \ [x] -> boat x
int "fields" = \ [x] -> fields x
int "tomato" = \ [x] -> tomato x; int "tomatoes" = int "tomato"


