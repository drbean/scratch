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

int "parent" = \args -> case args of 
	[x] -> isParent x
	[x,y] -> parented y x
int "parented" = \[x,y] -> parented y x
int "married"	= \ [x,y] -> married y x;	int "marry"	= int "married"
int "parents" = \ [x] -> isParent x
int "mother" = \ [x] -> isMother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "son" = \ [x] -> son x
int "brother" = \ [x] -> brother x

int "had" = \[x,y] -> have y x;	int "have" = int "had"
int "cut" = \args -> case args of [x,y] -> cut y x; [x,y,z] -> cut_with z y x
int "knew" = \[x,y] -> know y x; int "know" = int "knew"
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
int "ate" = \[x,y] -> eat y x; int "eat" = int "ate"

int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "got" = \[x,y,z] -> gave x y z; int "get" = int "got"
int "told" = \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x
int "tell" = int "told"
int "studied" = \args -> case args of
	[x,y] -> (studied_where y x || studied_what y x)
	[x,y,z] -> studied z y x
int "study" = int "studied"
int "went" = int "studied"
int "go" = int "went"

int "pumpkin_pie" = \[x] -> pumpkin_pie x
int "knife"	= \[x] -> knife x
int "missal"	= \[x] -> missal x
