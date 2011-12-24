module Story_Cats where

import Parsing

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "queen" "NP" [Thrd,Fem,Sg] []],
	[Cat "ann"	"NP" [Thrd,Fem,Sg] []],
	[Cat "debra"	"NP" [Thrd,Fem,Sg] []],
	[Cat "tanya"	"NP" [Thrd,Fem,Sg] []],
	[Cat "jennifer"	"NP" [Thrd,Fem,Sg] []],
	[Cat "the_state_of_colorado" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_gathering_place" "NP" [Thrd,Neutr,Sg] []],
	[Cat "nobody_or_nothing" "NP" [Thrd,Neutr,Sg] []]
	]

object_names = [

	[Cat "money"	"NP" [Thrd,Neutr,Sg] [],
		Cat "money"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "dress"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "dresses"	"CN" [Thrd,Neutr,Pl] []],

	[Cat "rent"	"CN" [Thrd,Neutr,Sg] [],
		Cat "rent"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "apartment"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "unemployment"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "ten_dollar_bill"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "birthday_card"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "sign"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "shelter"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "gift"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "lotion"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "smell"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "spirits"	"CN" [Thrd,Neutr,Pl] []],



	[Cat "administrative_assistant"	"NP" [Thrd,MascOrFem,Sg] []],
	[Cat "donator"	"NP" [Thrd,Masc,Sg] []],
	[Cat "supervisor"	"CN" [Thrd,Fem,Sg] []],
	[Cat "interviewee"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewees"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "visitor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "visitors"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "upbringing" "CN" [Thrd,Neutr,Sg] []],
	[Cat "language"	"CN" [Thrd,Neutr,Sg] []]
	]

story_verbs = [
	[Cat "looked"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "look"	"VP" [Infl] [Cat "_" "PP" [At] []]],
	[Cat "taught"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "taught"	"VP" [Tense] []],
	[Cat "teach"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "teach"	"VP" [Infl] []],
	[Cat "visited"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "visited"	"VP" [Tense] []],
	[Cat "visit"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "visit"	"VP" [Infl] []],
	[Cat "angered"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "anger"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offended"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offend"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "interviewed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "interview"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wore"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wear"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "wearing"	"GER" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

story_aux = [
	]

story_adjs = [
	[Cat "beautiful"	"ADJ" [] []],
	[Cat "black"	"ADJ" [] []],
	[Cat "white"	"ADJ" [] []]
	]


