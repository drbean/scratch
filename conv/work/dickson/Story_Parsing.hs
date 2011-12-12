module Story_Parsing where

import Parsing

collect_lex = [
	("auxiliary verbs",	aux),
	("interesting verbs",	story_verbs),
	("intransitive verbs",	intransitives),
	("transitive verbs",	transitives),
	("ditransitive verbs",	ditransitives),
	("object_names",	object_names),
	("class_names",	class_names),
	("prepositions",	preps),
	("determiners",	determiners),
	("possessives",	possessives)
	]

proper_names, object_names :: Lexset

proper_names = [
	[Cat "english" "NP" [Thrd,Neutr,Sg] []],
	[Cat "dee"	"NP" [Thrd,Fem,Sg] []],
	[Cat "alf"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [

	[Cat "uncle"	"NP" [Thrd,Masc,Sg] []],
	[Cat "nephew"	"NP" [Thrd,Masc,Sg] []],

	[Cat "superintendent"	"NP" [Thrd,Masc,Sg] []],
	[Cat "supervisor"	"NP" [Thrd,Masc,Sg] []],
	[Cat "apprentice"	"NP" [Thrd,MascOrFem,Sg] []],
	[Cat "vocational_school"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "construction"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "electrician"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "electricians"	"NP" [Thrd,Neutr,Pl] []],
	[Cat "interviewer"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "transformer"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "ship"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "ships"	"NP" [Thrd,Neutr,Pl] []],
	[Cat "shipyard"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "disappointment"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "money"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "upbringing"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "story"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "job"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "language"	"NP" [Thrd,Neutr,Sg] []]
	]

story_verbs = [
	[Cat "lift"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "resented"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "resent"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]


