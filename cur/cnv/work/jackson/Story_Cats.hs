module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["the","state","of","colorado"],	"the_state_of_colorado"),
	(["the","gathering","place"],	"the_gathering_place"),
	(["administrative","assistant"],	"administrative_assistant"),
	(["ten","dollar","bill"],	"ten_dollar_bill"),
	(["birthday","card"],	"birthday_card")
	]

names = [
	[Cat "queen" "NP" [Thrd,Fem,Sg] []],
	[Cat "ann"	"NP" [Thrd,Fem,Sg] []],
	[Cat "debra"	"NP" [Thrd,Fem,Sg] []],
	[Cat "tanya"	"NP" [Thrd,Fem,Sg] []],
	[Cat "jennifer"	"NP" [Thrd,Fem,Sg] []],
	[Cat "the_state_of_colorado" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_gathering_place" "NP" [Thrd,Neutr,Sg] []],
	[Cat "nobody_or_nothing" "NP" [Thrd,Neutr,Sg] []]
	]

nouns = [

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
	[Cat "lotion"	"NP" [Thrd,Neutr,Sg] [],
		Cat "lotion"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "smell"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "spirits"	"CN" [Thrd,Neutr,Pl] []],



	[Cat "donator"	"NP" [Thrd,Masc,Sg] []],
	[Cat "supervisor"	"CN" [Thrd,Fem,Sg] []],
	[Cat "supervisors"	"CN" [Thrd,Fem,Pl] []],
	[Cat "counseling"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "counselor"	"CN" [Thrd,Fem,Sg] []],
	[Cat "counselors"	"CN" [Thrd,Fem,Pl] []],
	[Cat "interviewee"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "interviewees"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "visitor"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "visitors"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "home"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "jobs"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "upbringing" "CN" [Thrd,Neutr,Sg] []]
	]

verbs = [
	[Cat "cried"	"VP" [Tense] []],
	[Cat "cry"	"VP" [Infl] []],
	-- [Cat "lost"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	-- [Cat "lose"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "volunteered"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "volunteer"	"VP" [Infl] [Cat "_" "PP" [At] []],
		Cat "volunteer"	"CN" [Thrd,MascOrFem,Sg] []],
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

aux = [
	]

adjs = [
	[Cat "older"	"ADJ" [] []],
	[Cat "homeless"	"ADJ" [] []],
	[Cat "beautiful"	"ADJ" [] []],
	[Cat "black"	"ADJ" [] []],
	[Cat "white"	"ADJ" [] []]
	]

advs = [
	]
