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
names = [
	[Cat "alex_tew" "NP" [Thrd,Masc,Sg] []]
	, [Cat "mark_zuckerberg" "NP" [Thrd,Masc,Sg] []]
	, [Cat "the_million_dollar_homepage" "NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_one_million_people_page"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "facebook"	"NP" [Thrd,Neutr,Sg] []]
	]

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "a"	"NP" [Thrd,Fem,Sg] []],
	[Cat "b"	"NP" [Thrd,Fem,Sg] []],
	[Cat "c"	"NP" [Thrd,Fem,Sg] []],
	[Cat "d"	"NP" [Thrd,Fem,Sg] []],
	[Cat "a-ho"	"NP" [Thrd,Fem,Sg] []],
	[Cat "ellen" "NP" [Thrd,Fem,Sg] []],
	[Cat "dr_bean" "NP" [Thrd,Masc,Sg] []],
	[Cat "steve" "NP" [Thrd,Masc,Sg] []],
	[Cat "european_campers" "NP" [Thrd,Neutr,Sg] []],
	[Cat "carrefour" "NP" [Thrd,Neutr,Sg] []],
	[Cat "charles" "NP" [Thrd,Masc,Sg] []],
	[Cat "jacques" "NP" [Thrd,Masc,Sg] []],
	[Cat "olivier" "NP" [Thrd,Masc,Sg] []],
	[Cat "todd" "NP" [Thrd,Masc,Sg] []],
	[Cat "cusp" "NP" [Thrd,Neutr,Sg] []],
	[Cat "slow_living" "NP" [Thrd,Neutr,Sg] []],
	[Cat "alan" "NP" [Thrd,Masc,Sg] []],
	[Cat "david" "NP" [Thrd,Masc,Sg] []],
	[Cat "dot" "NP" [Thrd,Fem,Sg] []],
	[Cat "tan" "NP" [Thrd,Fem,Sg] []],
	[Cat "nobody_or_nothing" "NP" [Thrd,Neutr,Sg] []]
nouns = [
	 [Cat "experiment"	"CN" [Thrd,Neutr,Sg] []]
	 , [Cat "good_idea"	"CN" [Thrd,Neutr,Sg] []]
	 , [Cat "a_good_price"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "money"   "NP" [Sg,Neutr,Thrd]   []]

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
	[Cat "salesman"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "sales_manager"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "customer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "production_manager"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "ceo"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "office_worker"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "doctor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "psychologist"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "teacher"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "adventurer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "adventurers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "jobs"	"CN" [Thrd,Neutr,Pl] []]
	]

verbs = [
	[Cat "helped"	"VP" [Tense] [],
		Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [],
		Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "grew"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "grow"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "bought"	"VP" [Tense] [],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "buy"	"VP" [Infl] [],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"VP" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []]],
	[Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []]],
	[Cat "paid"	"VP" [Tense] [],
		Cat "paid"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]],
	[Cat "pay"	"VP" [Infl] [],
		Cat "pay"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]
	[Cat "studied"	"VP" [Tense] [],
		Cat "studied"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "study"	"VP" [Infl] [],
		Cat "study"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [],
		Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [],
		Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "founded"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "found"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "set_up"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "promoted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "promoted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []]],
	[Cat "promote"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "promote"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []]],
	[Cat "bought"	"VP" [Tense] [],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "bought"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] [],
						Cat "_" "PP" [From] []]],
	[Cat "buy"	"VP" [Infl] [],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "buy"	"VP" [Infl] [ Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []],
		Cat "buy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] [],
						Cat "_" "PP" [From] []]],
	[Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "PP" [To] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sold"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
	[Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "PP" [To] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [On] []],
		Cat "sell"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] [],
						Cat "_" "PP" [On] []]],
	[Cat "paid"	"VP" [Tense] [],
		Cat "paid"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "paid" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]],
	[Cat "pay"	"VP" [Infl] [],
		Cat "pay"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "PP" [For] []]]

story_verbs = [
	[Cat "cried"	"VP" [Tense] []],
	[Cat "cry"	"VP" [Infl] []],
	[Cat "lost"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "lose"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "put_pressure"	"VP" [Tense] [Cat "_" "PP" [On] []],
		Cat "put_pressure"	"VP" [Infl] [Cat "_" "PP" [On] []]],
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
	[Cat "best"	"ADJ" [] []],
	[Cat "useful"	"ADJ" [] []]
	]


