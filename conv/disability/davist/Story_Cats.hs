module Story_Cats where

import Parsing

type WordConverted = ([String],String)

multipart_names :: [WordConverted]

multipart_names = [
	(["troop","409"],	"troop_409")
	]

proper_names, object_names, story_verbs :: Lexset

proper_names = [
	[Cat "connie"	"NP" [Thrd,Fem,Sg] []],
	[Cat "florida"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "pensacola"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "robert"	"NP" [Thrd,Masc,Sg] []],
	[Cat "tom"	"NP" [Thrd,Masc,Sg] []],
	[Cat "christopher"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [

	[Cat "accident"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "surfing_accident"	"CN" [Thrd,Neutr,Sg] []],

        [Cat "son"    "CN" [Sg,Masc,Thrd] []],
	[Cat "mother"	"CN" [Thrd,Fem,Sg] []],
	[Cat "father"	"CN" [Thrd,Masc,Sg] []],
	[Cat "husband"	"CN" [Thrd,Masc,Sg] []],
	[Cat "wife"	"CN" [Thrd,Fem,Sg] []],

	[Cat "disability"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "body"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "device"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "beach"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "horse"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "friends"	"CN" [Thrd,MascOrFem,Pl] []],
	[Cat "lack_of_depression"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "state"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "hometown"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "neck"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "water_sports"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "surfing"	"NP" [Thrd,Neutr,Sg] []],
	[Cat "ventilator"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "bowel_movement"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "bowel_movements"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "brain_damage"	"NP" [Thrd,Neutr,Sg] []],



	[Cat "story"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "name"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "names"	"CN" [Thrd,Neutr,Pl] []]
	]

story_verbs = [

	[Cat "appreciated"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "appreciate"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "breathed"	"VP" [Tense] [],
	--	Cat "breathed"	"VP" [Tense] [Cat "_" "PP" [With] []]],
	--[Cat "breathe"	"VP" [Infl] [],
	--	Cat "breathe"	"VP" [Infl] [Cat "_" "PP" [With] []]],
	[Cat "recovered"	"VP" [Tense] []],
	[Cat "recover"	"VP" [Infl] []],
	[Cat "used"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "use"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "broke"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "break"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "relied"	"VP" [Tense] [Cat "_" "PP" [On] []]],
	[Cat "rely"	"VP" [Infl] [Cat "_" "PP" [On] []]],
	[Cat "cared_for"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "cared_for"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "care_for"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "care_for"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "looked_after"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "looked_after"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "look_after"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "look_after"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "took_care_of"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "took_care_of"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "take_care_of"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "take_care_of"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [After] []]],
	[Cat "helped"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "help"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "removed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "remove"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greeted"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "greet"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "answered"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "answer"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "played"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "play"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "embarrassed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "embarrass"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offended"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "offend"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "annoyed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "annoy"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	-- [Cat "lost"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	-- [Cat "lose"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "felt"	"VP" [Tense] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "felt"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "feel"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "feel"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "sounded"	"VP" [Tense] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "sounded"	"VP" [Tense] [Cat "_" "PP" [Like] []]],
	[Cat "sound"	"VP" [Infl] [Cat "_" "ADJ" [AccOrDat] []],
		Cat "sound"	"VP" [Infl] [Cat "_" "PP" [Like] []]],

	[Cat "went"	"VP" [Tense] [Cat "_" "PP" [To] []]],
	[Cat "go"	"VP" [Infl] [Cat "_" "PP" [To] []]],
	[Cat "grew_up"	"VP" [Tense] [Cat "_" "PP" [In] []]],
	[Cat "grow_up"	"VP" [Infl] [Cat "_" "PP" [In] []]],
	[Cat "came"	"VP" [Tense] [Cat "_" "PP" [From] []]],
	[Cat "come"	"VP" [Infl] [Cat "_" "PP" [From] []]],
	[Cat "slept"	"VP" [Tense] [Cat "_" "PP" [With] []],
		Cat "slept"	"VP" [Tense] [ Cat "_" "PP" [In] [],
						Cat "_" "PP" [With] []]],
	[Cat "sleep"	"VP" [Infl] [Cat "_" "PP" [With] []],
		Cat "sleep"	"VP" [Infl] [Cat "_" "PP" [In] [],
						Cat "_" "PP" [With] []]],
	[Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [In] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [For] []],
		Cat "live"	"VP" [Infl] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] [],
						Cat "_" "PP" [For] []]],
	[Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [In] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [For] []],
		Cat "lived"	"VP" [Tense] [Cat "_" "PP" [With] [],
						Cat "_" "PP" [In] [],
						Cat "_" "PP" [For] []]],
	[Cat "said"	"VP" [Tense] [Cat "_" "S" [] []],
		Cat "said"	"VP" [Tense] [Cat "_" "WH" [] []]],
	[Cat "say"	"VP" [Infl] [Cat "_" "S" [] []],
		Cat "say"	"VP" [Infl] [Cat "_" "WH" [] []]],
	[Cat "asked"	"VP" [Tense] [Cat "_" "NP" [] []]],
	[Cat "ask"	"VP" [Infl] [Cat "_" "NP" [] []]],
	[Cat "stayed"	"VP" [Tense] []],
	[Cat "stay"	"VP" [Infl] []],
	[Cat "left"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "leave"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "turned"	"VP" [Tense] [Cat "_" "ADV" [Around] []]],
	[Cat "turn"	"VP" [Infl] [Cat "_" "ADV" [Around] []]],
	[Cat "thought"	"VP" [Tense] [Cat "_" "S" [] []]],
	[Cat "think"	"VP" [Infl] [Cat "_" "S" [] []]]

	]

story_aux = [
	[Cat "could" "AUX" [] []],
	[Cat "couldn't" "AUX" [] []]

	]

story_adjs = [
	[Cat "paralyzed"	"ADJ" [AccOrDat] []],
	[Cat "broken"	"ADJ" [AccOrDat] []],
	[Cat "brain-damaged"	"ADJ" [AccOrDat] []],
	[Cat "mentally-disabled"	"ADJ" [AccOrDat] []],
	[Cat "physically-disabled"	"ADJ" [AccOrDat] []],
	[Cat "disabled"	"ADJ" [AccOrDat] []],
	[Cat "happy"	"ADJ" [] []],
	[Cat "good"	"ADJ" [] []],
	[Cat "kind"	"ADJ" [] []]
	]

story_advs = [
	[Cat "hands_on_hips"	"ADV" [] []]
	]
