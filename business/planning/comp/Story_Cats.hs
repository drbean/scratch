module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	]

nouns = [
	[Cat "teacher"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "student"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "students"	"NP" [Thrd,MascOrFem,Sg] []]

	, [Cat "board"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "english"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "question"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "questions"	"NP" [Thrd,Neutr,Pl] []]


	, [Cat "clear_and_simple_idea"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "autonomy"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "ownership"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "innovation"	"NP" [Thrd,Neutr,Sg] []]


	]

verbs = [
	[Cat "see"    "V" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "saw"    "V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "liked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "like"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "bought"	"V" [Tense] [],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In] []],
		Cat "bought"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []]]
	, [Cat "buy"	"V" [Infl] [],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "buy"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In] []],
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
	[Cat "500_nt_and_up"	"ADV" [] []]
	, [Cat "100-800_nt"	"ADV" [] []]
	, [Cat "1_200_nt_and_up"	"ADV" [] []]
	, [Cat "1_000_nt_and_up"	"ADV" [] []]
	, [Cat "too_far"	"ADV" [] []]
	]
