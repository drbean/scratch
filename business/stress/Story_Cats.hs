module Story_Cats where

import Parsing

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["office","worker"],	"office_worker"),
	(["European","Campers"],	"European_Campers"),
	(["Dr","Bean"],	"Dr_Bean"),
	(["production","manager"],	"production_manager"),
	(["sales","manager"],	"sales_manager"),
	(["Slow","Living"],	"Slow_Living")
	]

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "A"	"NP" [Thrd,Fem,Sg] []],
	[Cat "B"	"NP" [Thrd,Fem,Sg] []],
	[Cat "C"	"NP" [Thrd,Fem,Sg] []],
	[Cat "D"	"NP" [Thrd,Fem,Sg] []],
	[Cat "A-ho"	"NP" [Thrd,Fem,Sg] []],
	[Cat "Ellen" "NP" [Thrd,Fem,Sg] []],
	[Cat "Dr_Bean" "NP" [Thrd,Masc,Sg] []],
	[Cat "steve" "NP" [Thrd,Masc,Sg] []],
	[Cat "European_Campers" "NP" [Thrd,Neutr,Sg] []],
	[Cat "Carrefour" "NP" [Thrd,Neutr,Sg] []],
	[Cat "Charles" "NP" [Thrd,Masc,Sg] []],
	[Cat "Jacques" "NP" [Thrd,Masc,Sg] []],
	[Cat "Olivier" "NP" [Thrd,Masc,Sg] []],
	[Cat "Todd" "NP" [Thrd,Masc,Sg] []],
	[Cat "CUSP" "NP" [Thrd,Neutr,Sg] []],
	[Cat "Slow_Living" "NP" [Thrd,Neutr,Sg] []],
	[Cat "Alan" "NP" [Thrd,Masc,Sg] []],
	[Cat "David" "NP" [Thrd,Masc,Sg] []],
	[Cat "Dot" "NP" [Thrd,Fem,Sg] []],
	[Cat "Tan" "NP" [Thrd,Fem,Sg] []],
	[Cat "Nobody_or_Nothing" "NP" [Thrd,Neutr,Sg] []]
	]

object_names = [

	[Cat "control"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "lack_of_control"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "uncertainty"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "support"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "lack_of_support"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "pressure"	"NP" [Thrd,Neutr,Sg] []],

	[Cat "company"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "unemployment"	"NP" [Thrd,Neutr,Sg] []],

	[Cat "supervisor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "supervisors"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "boss"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "bosses"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "subordinate"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "subordinates"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "employee"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "employees"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "manager"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "managers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "jobs"	"CN" [Thrd,Neutr,Pl] []]
	]

story_verbs = [
	[Cat "cried"	"VP" [Tense] []],
	[Cat "cry"	"VP" [Infl] []],
	[Cat "lost"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "lose"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "volunteered"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "volunteer"	"VP" [Infl] [Cat "_" "PP" [At] []]],
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
	[Cat "angry"	"ADJ" [] []],
	[Cat "stressful"	"ADJ" [] []],
	[Cat "brilliant"	"ADJ" [] []],
	[Cat "large"	"ADJ" [] []],
	[Cat "rude"	"ADJ" [] []],
	[Cat "best"	"ADJ" [] []]
	]


