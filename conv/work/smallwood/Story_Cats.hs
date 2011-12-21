module Story_Cats where

import Parsing

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "the_united_states" "NP" [Thrd,Neutr,Sg] []],
	[Cat "rutgers_university" "NP" [Thrd,Neutr,Sg] []],
	[Cat "tia"	"NP" [Thrd,Fem,Sg] []],
	[Cat "christine"	"NP" [Thrd,Fem,Sg] []],
	[Cat "steven"	"NP" [Thrd,Masc,Sg] []],
	[Cat "mr_payne"	"NP" [Thrd,Masc,Sg] []],
	[Cat "mr_batchelor"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [

	[Cat "money"	"NP" [Thrd,Neutr,Sg] [],
		Cat "money"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "dress"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "dresses"	"CN" [Thrd,Neutr,Pl] []],

	[Cat "boss"	"CN" [Thrd,Fem,Sg] []],
	[Cat "supervisor"	"CN" [Thrd,Fem,Sg] []],
	[Cat "receptionist"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "receptionists"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "interviewer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "interviewee"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewees"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "visitor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "visitors"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "company"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hospital"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "school"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "finance"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "accounting"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "business_law"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "subject"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "subjects"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "language"	"CN" [Thrd,Neutr,Sg] []]
	]

story_verbs = [
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
	[Cat "offensive"	"ADJ" [] []],
	[Cat "red"	"ADJ" [] []]
	]


