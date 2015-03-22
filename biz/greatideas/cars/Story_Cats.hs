module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "steve_wynn" "NP" [Thrd,Masc,Sg] []]
	, [Cat "the_ferrari_showroom" "NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "pepsi" "NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "punjabi_farmers" "NP" [Thrd,MascOrFem,Pl] []]
	-- , [Cat "the_punjabi_government"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "alex_tew"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "the_million_dollar_homepage"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "facebook"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "mark_zuckerberg"	"NP" [Thrd,Masc,Sg] []]
	]

nouns = [
	[Cat "visitor"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "visitors"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "car" "CN" [Thrd,Neutr,Sg] []]
	, [Cat "cars" "CN" [Thrd,Neutr,Pl] []]
	, [Cat "ten_dollars" "NP" [Thrd,Neutr,Sg] []]
	, [Cat "entrance_fee" "CN" [Thrd,Neutr,Sg] []]
	, [Cat "experiment" "CN" [Thrd,Neutr,Sg] []]
	
	-- [Cat "oranges"	"CN" [Thrd,Neutr,Pl] []],
	-- [Cat "website"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "site"	"CN" [Thrd,Neutr,Sg] []]
	]

verbs = [
	-- [Cat "offered" "VP" [Tense] [Cat "_" "INF" [To] []]],
	-- [Cat "offer" "VP" [Infl] [Cat "_" "INF" [To] []]],
	[Cat "started" "VP" [Tense] [Cat "_" "INF" [To] []]],
	[Cat "start" "VP" [Infl] [Cat "_" "INF" [To] []]],
	[Cat "looked" "VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "look" "VP" [Infl] [Cat "_" "PP" [At] []]],
	[Cat "entered" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "enter" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "paid"	"VP" [Tense] [],
		Cat "paid"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []]],
	[Cat "pay"	"VP" [Infl] [],
		Cat "pay"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []]],
        [Cat "charge" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "charge" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []]]

	]

aux = [
	]

adjs = [
	-- [Cat "ready"	"ADJ" [] [Cat "_" "INF" [To]       []]],
	-- [Cat "lots_of"	"ADJ" [] []],
	-- [Cat "a_lot_of"	"ADJ" [] []],
	-- [Cat "popular"	"ADJ" [] []],
	[Cat "successful"	"ADJ" [] []]
	, [Cat "unsuccessful"	"ADJ" [] []]
	-- [Cat "good"	"ADJ" [] []]
	]

advs = [
	-- [Cat "slowly"	"ADV" [] []]
	]
