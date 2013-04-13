module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "quanjiafu"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "miaoli"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "toufen"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "zhunan"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "yingtsailu"	"NP" [Thrd,Neutr,Sg] []]
	]

nouns = [
	[Cat "shoes"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "jogging_shoes"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "men's_formal_shoes"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "women's_formal_shoes"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "slippers"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "shoe_store"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "1_000_nt_and_up"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "1_200_nt_and_up"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "500_nt_and_up"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "800_nt"	"NP" [Thrd,Neutr,Sg] []]


	, [Cat "teacher"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "a_good_price"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "money"   "NP" [Sg,Neutr,Thrd]   []]
	, [Cat "product"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "price"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "place"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "promotion"	"NP" [Thrd,Neutr,Sg] []]

	]

verbs = [
	[Cat "see"    "V" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "saw"    "V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "liked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "like"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "bought"	"V" [Tense] [],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"V" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]]
	, [Cat "buy"	"V" [Infl] [],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"V" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]]
	, [Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"V" [Tense] [Cat "_" "PP" [To] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sold"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]]
	, [Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"V" [Infl] [Cat "_" "PP" [To] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sell"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]]
	, [Cat "paid"	"V" [Tense] [],
		Cat "paid"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "paid" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]
	, [Cat "pay"	"V" [Infl] [],
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
	, [Cat "good"	"ADJ" [] []]
	, [Cat "bad"	"ADJ" [] []]
	]

advs = [
	[Cat "500_NT_and_up"	"ADV" [] []]
	, [Cat "100-800_NT"	"ADV" [] []]
	, [Cat "1,200_NT_and_up"	"ADV" [] []]
	, [Cat "1,000_NT_and_up"	"ADV" [] []]
	, [Cat "too_far"	"ADV" [] []]
	]
