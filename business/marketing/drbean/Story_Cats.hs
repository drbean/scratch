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

	, [Cat "a_good_price"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "money"   "NP" [Sg,Neutr,Thrd]   []]
	]

verbs = [
	[Cat "liked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "like"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "bought"	"V" [Tense] [],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"V" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]],
	[Cat "buy"	"V" [Infl] [],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"V" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]],
	[Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"V" [Tense] [Cat "_" "PP" [To] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
	[Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"V" [Infl] [Cat "_" "PP" [To] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
	[Cat "paid"	"V" [Tense] [],
		Cat "paid"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]],
	[Cat "pay"	"V" [Infl] [],
		Cat "pay"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "pay" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
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
