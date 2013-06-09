module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	]

nouns = [
	[Cat "teacher"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "student"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "students"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "group"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "groups"	"CN" [Thrd,Neutr,Pl] []]

	, [Cat "member"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "members"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "loser"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "losers"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "winner"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "winners"	"CN" [Thrd,MascOrFem,Pl] []]


	, [Cat "board"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "english"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "question"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "questions"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "answer"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "answers"	"CN" [Thrd,Neutr,Pl] []]


	, [Cat "activity"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "compcomp_activity"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "ingredient_for_success"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "ingredients_for_success"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "framework"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "clear_and_simple_idea"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "autonomy"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "ownership"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "innovation"	"NP" [Thrd,Neutr,Sg] []]


	]

verbs = [
	[Cat "liked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "like"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "asked"	"V" [Tense] [],
		Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In] []],
		Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []],
		Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]
	, [Cat "ask"	"V" [Infl] [],
		Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In] []],
		Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [At] []],
		Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]
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
	, [Cat "answered"	"V" [Tense] [],
		Cat "answered"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "answered" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "answered" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "answered" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]
	, [Cat "answer"	"V" [Infl] [],
		Cat "answer"	"V" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "answer" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "answer" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "answer" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]
        , [Cat "talked" "V" [Tense] [],
                Cat "talked"    "V" [Tense] [Cat "_" "PP" [To] []],
                Cat "talked"    "V" [Tense] [Cat "_" "PP" [About] []],
                Cat "talked"    "V" [Tense] [Cat "_" "PP" [In] []],
                Cat "talked"    "V" [Tense] [Cat "_" "PP" [To] [],
                                        Cat "_" "PP" [About] []],
                Cat "talked"    "V" [Tense] [Cat "_" "PP" [To] [],
                                        Cat "_" "PP" [In] []]]
        , [Cat "talk"   "V" [Infl]  [],
                Cat "talk"      "V" [Infl]  [Cat "_" "PP" [To] []],
                Cat "talk"      "V" [Infl] [Cat "_" "PP" [About] []],
                Cat "talk"      "V" [Infl] [Cat "_" "PP" [In] []],
                Cat "talk"      "V" [Infl] [Cat "_" "PP" [To] [],
                                        Cat "_" "PP" [About] []],
                Cat "talk"      "V" [Infl] [Cat "_" "PP" [To] [],
                                        Cat "_" "PP" [In] []]]
	]

aux = [
	]

adjs = [
	[Cat "successful"	"ADJ" [] []]
	, [Cat "unsuccessful"	"ADJ" [] []]
	, [Cat "good"	"ADJ" [] []]
	, [Cat "bad"	"ADJ" [] []]
	, [Cat "innovative"	"ADJ" [] []]
	]

advs = [
	[Cat "500_nt_and_up"	"ADV" [] []]
	, [Cat "100-800_nt"	"ADV" [] []]
	, [Cat "1_200_nt_and_up"	"ADV" [] []]
	, [Cat "1_000_nt_and_up"	"ADV" [] []]
	, [Cat "too_far"	"ADV" [] []]
	]
