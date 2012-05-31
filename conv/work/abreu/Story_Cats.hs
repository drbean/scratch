module Story_Cats where

import Parsing

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "english" "NP" [Thrd,Neutr,Sg] []],
	[Cat "spanish" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_dominican_republic" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_united_states" "NP" [Thrd,Neutr,Sg] []],
	[Cat "boston" "NP" [Thrd,Neutr,Sg] []],
	[Cat "boston_university" "NP" [Thrd,Neutr,Sg] []],
	[Cat "joan"	"NP" [Thrd,Fem,Sg] []],
	[Cat "adela"	"NP" [Thrd,Fem,Sg] []],
	[Cat "claritza"	"NP" [Thrd,Fem,Sg] []],
	[Cat "john_doe"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [

	[Cat "money"	"NP" [Thrd,Neutr,Sg] [],
		Cat "money"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "information_technology"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "i_t"	"NP" [Thrd,Neutr,Sg] []],

	[Cat "aunt"	"CN" [Thrd,Fem,Sg] []],
	[Cat "niece"	"CN" [Thrd,Fem,Sg] []],
	[Cat "uncle"	"CN" [Thrd,Masc,Sg] []],
	[Cat "nephew"	"CN" [Thrd,Masc,Sg] []],

	[Cat "boss"	"CN" [Thrd,Fem,Sg] []],
	[Cat "supervisor"	"CN" [Thrd,Fem,Sg] []],
	[Cat "receptionist"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "receptionists"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "customer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "customers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "visitor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "visitors"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "company"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hospital"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "school"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "upbringing"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "language"	"CN" [Thrd,Neutr,Sg] []]
	]

story_verbs = [
	[Cat "visited"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "visited"	"VP" [Tense] []],
	[Cat "visit"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "visit"	"VP" [Infl] []],
	[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "spelled"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "spell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

story_aux = [
	[Cat "could"    "AUX" [] []],
	[Cat "couldn't" "AUX" [] []]
	]

story_adjs = [
	[Cat "demanding"	"ADJ" [AccOrDat] []],
	[Cat "scared"	"ADJ" [] []]
	]

story_advs = [
	[Cat "hands_on_hips"	"ADV" [] []]
	]
