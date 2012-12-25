module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "alex_tew" "NP" [Thrd,Masc,Sg] []]
	, [Cat "mark_zuckerberg" "NP" [Thrd,Masc,Sg] []]
	, [Cat "the_million_dollar_homepage" "NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_one_million_people_page"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "facebook"	"NP" [Thrd,Neutr,Sg] []]
	]

nouns = [
	 [Cat "experiment"	"CN" [Thrd,Neutr,Sg] []]
	 , [Cat "good_idea"	"CN" [Thrd,Neutr,Sg] []]
	 , [Cat "a_good_price"	"NP" [Thrd,Neutr,Sg] []]
	]

verbs = [
	[Cat "helped"	"VP" [Tense] [],
		Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [],
		Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "founded"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "found"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "bought"	"VP" [Tense] [],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "buy"	"VP" [Infl] [],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"VP" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []]],
	[Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []]],
	[Cat "paid"	"VP" [Tense] [],
		Cat "paid"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]],
	[Cat "pay"	"VP" [Infl] [],
		Cat "pay"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]

	]

aux = [
	]

adjs = [
	[Cat "successful"	"ADJ" [] []]
	, [Cat "unsuccessful"	"ADJ" [] []]
	]

advs = [
	-- [Cat "slowly"	"ADV" [] []]
	]
