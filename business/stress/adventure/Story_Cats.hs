module Story_Cats where

import Parsing

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["Dr","Bean"],	"Dr_Bean")
	]

names, nouns, verbs :: Lexset

names = [
	[Cat "cusp" "NP" [Thrd,Neutr,Sg] []],
	[Cat "ellen_macarthur" "NP" [Thrd,Fem,Sg] []],
	[Cat "dr_bean" "NP" [Thrd,Masc,Sg] []],
	[Cat "steve_fossett" "NP" [Thrd,Masc,Sg] []]
	]

nouns = [

	[Cat "control"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "lack_of_control"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "uncertainty"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "support"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "lack_of_support"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "pressure"	"NP" [Thrd,Neutr,Sg] []],

	[Cat "team"	"CN" [Thrd,MascOrFem,Sg] []],

	[Cat "teacher"	"CN" [Thrd,Masc,Sg] []],
	[Cat "adventurer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "adventurers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "boat"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "aircraft"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "plane"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "powered_aircraft"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "glider"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "balloon"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "world"	"CN" [Thrd,Neutr,Sg] []]
	]

verbs = [
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "felt_stress"	"VP" [Tense] [Cat "_" "PP" [BecauseOf] []],
		Cat "felt_stress"	"VP" [Tense] []],
	[Cat "feel_stress"	"VP" [Infl] [Cat "_" "PP" [BecauseOf] []],
		Cat "feel_stress"	"VP" [Infl] []],
	[Cat "put_pressure"	"VP" [Tense] [Cat "_" "PP" [On] []],
		Cat "put_pressure"	"VP" [Infl] [Cat "_" "PP" [On] []]],
	[Cat "flew"	"VP" [Tense] [Cat "_" "PP" [Around] []],
		Cat "flew"	"VP" [Tense] [Cat "_" "PP" [Around] [],
						Cat "_" "PP" [In] []]],
	[Cat "fly"	"VP" [Infl] [Cat "_" "PP" [Around] []],
		Cat "fly"	"VP" [Infl] [Cat "_" "PP" [Around] [],
						Cat "_" "PP" [In] []]],
	[Cat "sailed"	"VP" [Tense] [Cat "_" "PP" [Around] []],
		Cat "sailed"	"VP" [Tense] [Cat "_" "PP" [Around] [],
						Cat "_" "PP" [In] []],
		Cat "sailed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "sail"	"VP" [Infl] [Cat "_" "PP" [Around] []],
		Cat "sail"	"VP" [Infl] [Cat "_" "PP" [Around] [],
						Cat "_" "PP" [In] []],
		Cat "sail"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

aux = [
	]

adjs = [
	[Cat "stressful"	"ADJ" [] []]
	]

advs = [
	-- [Cat "slowly"	"ADV" [] []]
	]
