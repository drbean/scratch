module Story_Cats where

import Parsing

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["troop","409"],	"troop_409")
	]

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "amanda"	"NP" [Thrd,Fem,Sg] []],
	[Cat "rita"	"NP" [Thrd,Fem,Sg] []],
	[Cat "south_africa"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "new_york"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "california"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "michelle"	"NP" [Thrd,Fem,Sg] []],
	[Cat "mia"	"NP" [Thrd,Fem,Sg] []],
	[Cat "renee"	"NP" [Thrd,Fem,Sg] []]
	]

object_names = [

        [Cat "mother"    "CN" [Sg,Fem,Thrd] []],
        [Cat "daughter"    "CN" [Sg,Fem,Thrd] []],
	[Cat "daughters"    "CN" [Pl,Fem,Thrd] []],
	[Cat "cancer"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "throat_cancer"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "disability"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "shrinking_violet"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "movie_star"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "telemarketer"	"CN" [Thrd,MascOrFem,Sg] []],
	[Cat "telemarketers"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "joke"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "side"	"CN" [Thrd,Neutr,Sg] [],
		Cat "sides"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "electrolarynx"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "computer"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "front_desk"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "restaurant"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "night"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "operation"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "bed"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hospital"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "voice"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "sound"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "phone"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "gym"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "year"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hips"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []]
	]

story_verbs = [
	[Cat "thought"	"VP" [Tense] [Cat "_" "S" [] []]],
	[Cat "think"	"VP" [Infl] [Cat "_" "S" [] []]],
	[Cat "turned"	"VP" [Tense] [Cat "_" "ADV" [Around] []]],
	[Cat "turn"	"VP" [Infl] [Cat "_" "ADV" [Around] []]],
	[Cat "left"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "leave"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "stayed"	"VP" [Tense] []],
	[Cat "stay"	"VP" [Infl] []],
	[Cat "asked"	"VP" [Tense] [Cat "_" "NP" [] []]],
	[Cat "ask"	"VP" [Infl] [Cat "_" "NP" [] []]],
	[Cat "said"	"VP" [Tense] [Cat "_" "S" [] []],
		Cat "said"	"VP" [Tense] [Cat "_" "WH" [] []]],
	[Cat "say"	"VP" [Infl] [Cat "_" "S" [] []],
		Cat "say"	"VP" [Infl] [Cat "_" "WH" [] []]],
	[Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [In] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [For] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] [],
						Cat "_" "PP" [For] []]],
	[Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [In] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [For] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] [],
						Cat "_" "PP" [For] []]],
	[Cat "slept"	"VP" [Tense] [Cat "_" "PP" [With] []],
		Cat "slept"	"VP" [Tense] [ Cat "_" "PP" [In] [],
						Cat "_" "PP" [With] []]],
	[Cat "sleep"	"VP" [Infl] [Cat "_" "PP" [With] []],
		Cat "sleep"	"VP" [Infl] [Cat "_" "PP" [In] [],
						Cat "_" "PP" [With] []]],
	[Cat "came"	"VP" [Tense] [Cat "_" "PP" [From] []]],
	[Cat "come"	"VP" [Infl] [Cat "_" "PP" [From] []]],
	[Cat "went"	"VP" [Tense] [Cat "_" "PP" [To] []]],
	[Cat "go"	"VP" [Infl] [Cat "_" "PP" [To] []]],
	[Cat "sounded"	"VP" [Tense] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "sounded"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "sound"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "sound"	"VP" [Infl] [Cat "_" "PP" [Like] []]],
	[Cat "feel"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "sound"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "felt"	"VP" [Tense] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "felt"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "feel"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "feel"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	-- [Cat "lost"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	-- [Cat "lose"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "annoyed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "annoy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offended"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offend"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "embarrassed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "embarrass"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "played"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "play"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "answered"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "answer"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

story_aux = [
	]

story_adjs = [
	[Cat "around"	"ADJ" [] [],
		Cat "around"	"ADV" [] []],
	[Cat "scared"	"ADJ" [AccOrDat] []],
	[Cat "disabled"	"ADJ" [AccOrDat] []],
	-- [Cat "blessed"	"ADJ" [] []],
	[Cat "happy"	"ADJ" [] []],
	[Cat "funny"	"ADJ" [] []],
	[Cat "good"	"ADJ" [] []],
	[Cat "kind"	"ADJ" [] []]
	]

story_advs = [
	[Cat "hands_on_hips"	"ADV" [] []]
	]
