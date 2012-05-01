module Story_Cats where

import Parsing

proper_names, object_names :: Lexset

proper_names = [
	[Cat "english" "NP" [Thrd,Neutr,Sg] []],
	[Cat "dee"	"NP" [Thrd,Fem,Sg] []],
	[Cat "alf"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [

	[Cat "uncle"	"CN" [Thrd,Masc,Sg] []],
	[Cat "nephew"	"CN" [Thrd,Masc,Sg] []],

	[Cat "superintendent"	"CN" [Thrd,Masc,Sg] []],
	[Cat "supervisor"	"CN" [Thrd,Masc,Sg] []],
	[Cat "apprentice"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "vocational_school"	"NP" [Thrd,Neutr,Sg] [],
		Cat "vocational_school"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "construction"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "electrician"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "electricians"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "interviewer"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "transformer"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "ship"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "ships"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "shipyard"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "disappointment"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "money"	"NP" [Thrd,Neutr,Sg] [],
		Cat "money"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "upbringing"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "language"	"CN" [Thrd,Neutr,Sg] []]
	]

story_verbs = [
	[Cat "lift"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "resented"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "resent"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

story_aux = [
	]

story_adjs = [
	]

story_advs = [
	]
