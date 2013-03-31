module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "quanlian"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "quanjiafu"	"NP" [Thrd,Neutr,Sg] []]
	]

nouns = [
	[Cat "shoes"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "oil"	"NP" [Thrd,Neutr,Sg] [],
		Cat "oil"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "milk"	"NP" [Thrd,Neutr,Sg] [],
		Cat "milk"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "bananas"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "eggs"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "rice"	"NP" [Thrd,Neutr,Sg] [],
		Cat "rice"	"CN" [Thrd,Neutr,Sg] []]

	, [Cat "experiment"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "good_idea"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "a_good_price"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "advertisers"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "advertising_space"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "media"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "radio_and_television"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "website"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "business_management"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "money"   "NP" [Sg,Neutr,Thrd]   []]
	]

verbs = [
	[Cat "studied"	"VP" [Tense] [],
		Cat "studied"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "study"	"VP" [Infl] [],
		Cat "study"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [],
		Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [],
		Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "founded"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "found"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "promoted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "promoted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []]],
	[Cat "promote"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "promote"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []]],
	[Cat "liked"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "liked"	"VP" [Tense] [Cat "_" "PP" [From] []],
		Cat "liked"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "like"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "like"	"VP" [Infl] [Cat "_" "PP" [From] []],
		Cat "like"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "bought"	"VP" [Tense] [],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]],
	[Cat "buy"	"VP" [Infl] [],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"VP" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]],
	[Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "PP" [To] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
	[Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "PP" [To] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
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
