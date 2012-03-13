module Story_Cats where

import Parsing

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["troop","409"],	"troop_409")
	]

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "derek" "NP" [Thrd,Masc,Sg] []],
	[Cat "richard"	"NP" [Thrd,Masc,Sg] []],
	[Cat "claudia"	"NP" [Thrd,Fem,Sg] []],
	[Cat "randy"	"NP" [Thrd,Masc,Sg] []],
	[Cat "troop_409" "NP" [Thrd,Masc,Sg] []],
	[Cat "nobody_or_nothing_or_nowhere" "NP" [Thrd,Neutr,Sg] []]
	]

object_names = [

	[Cat "scout"	"CN" [Thrd,Masc,Sg] []],
	[Cat "scouts"	"CN" [Thrd,Masc,Pl] []],
	[Cat "scoutmaster"	"CN" [Thrd,Masc,Sg] []],
	[Cat "assistant_scoutmaster"	"CN" [Thrd,Fem,Sg] []],
	[Cat "eagle_scout"	"CN" [Thrd,Masc,Sg] []],
	[Cat "leader"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "leaders"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "troop"	"CN" [Thrd,Masc,Sg] [],
		Cat "troop"	"CN" [Thrd,Masc,Pl] []],
	[Cat "traffic_accident"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "brain_damage"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "job"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "jobs"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []]
	]

story_verbs = [
	[Cat "led"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "led"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "volunteered"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	[Cat "volunteer"	"VP" [Infl] [Cat "_" "PP" [At] []]]
	--[Cat "looked"	"VP" [Tense] [Cat "_" "PP" [At] []]],
	--[Cat "look"	"VP" [Infl] [Cat "_" "PP" [At] []]],
	--[Cat "taught"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "taught"	"VP" [Tense] []],
	--[Cat "teach"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "teach"	"VP" [Infl] []],
	--[Cat "interviewed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "interview"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

story_aux = [
	]

story_adjs = [
	[Cat "female"	"ADJ" [] []],
	[Cat "male"	"ADJ" [] []],
	[Cat "older"	"ADJ" [] []],
	[Cat "dedicated"	"ADJ" [] []],
	[Cat "disabled"	"ADJ" [] []],
	[Cat "physically-disabled"	"ADJ" [] []],
	[Cat "mentally-disabled"	"ADJ" [] []],
	[Cat "developmentally-disabled"	"ADJ" [] []],
	[Cat "mentally-handicapped"	"ADJ" [] []],
	[Cat "mentally-retarded"	"ADJ" [] []]
	]


