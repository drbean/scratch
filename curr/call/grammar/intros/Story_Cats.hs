module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "alex"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "dave"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "jeff"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "neil"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "shane"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "cindy"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "kelly"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "avril_lavigne"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "mindy"	"NP" [Thrd,Fem,Sg] []]
	-- , [Cat "rena"	"NP" [Thrd,Fem,Sg] []]
	-- , [Cat "vicky"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "applied_foreign_languages"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "english"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "minghsin_university"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "america"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "taiwan"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "hsinchu"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "taoyuan"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "jiayi"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "april_30th,_1994"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "1994"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "lextar"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "japanese"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "nantou"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "jeremy_lin"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "hukou"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "gemitek"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "hsiao_ching-teng"	"NP" [Thrd,Masc,Sg] []]

	]

nouns = [
	[Cat "teacher"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "student"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "students"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "mother"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "father"	"CN" [Thrd,Masc,Sg] []]
	, [Cat "sister"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "sisters"	"CN" [Thrd,Fem,Pl] []]
	, [Cat "brother"	"CN" [Thrd,Masc,Sg] []]
	, [Cat "brothers_and_sisters"	"CN" [Thrd,MascOrFem,Pl] []]

	, [Cat "cat"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "cats"	"CN" [Thrd,Neutr,Pl] []]
	, [Cat "music"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "hello_kitty"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_color_pink"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "mi_mi"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "basketball"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "exercising"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "drawing"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "listening_to_music"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "reading"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "travel"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "grandmother"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "farmer"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "career_woman"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "truck_driver"	"CN" [Thrd,Masc,Sg] []]

	, [Cat "design_assistant"	"CN" [Thrd,MascOrFem,Sg] []]

	, [Cat "24"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "playing_the_piano"	"NP" [Thrd,Neutr,Sg] []]

	, [Cat "babysitter"	"CN" [Thrd,Fem,Sg] []]
	, [Cat "shopping"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "weekends"	"NP" [Thrd,Neutr,Pl] []]

-- mindy
	, [Cat "the_tv_program_discovery"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_song_memory"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "going_to_the_movies"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "a_japanese_interpreter"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "pizza"	"NP" [Thrd,Neutr,Sg] []]

--kelly
	, [Cat "watching_tv"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "a_dietitian"	"NP" [Thrd,MascOrFem,Sg] []]

-- neil
	, [Cat "27"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "non-commissioned_officer"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "jogging"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "swimming"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_military"	"NP" [Thrd,Neutr,Sg] []]

-- shane
	, [Cat "21"	"NP" [Thrd,MascOrFem,Sg] []]
	, [Cat "christian"	"CN" [Thrd,MascOrFem,Sg] []
		, Cat "christian" "ADJ" [] []]
	, [Cat "singing"	"NP" [Thrd,Neutr,Sg] []]

-- dave
	, [Cat "making_friends"	"NP" [Thrd,Neutr,Sg] []]
	]

verbs = [
	[Cat "born"	"V" [Part] []
	, Cat "born"	"V" [Part] [Cat "_" "PP" [In] []]
	, Cat "born"	"V" [Part] [Cat "_" "PP" [In] [], Cat "_" "PP" [In] []]]
	, [Cat "liked" "V" [Tense] [Cat "_" "NP" [AccOrDat] []] ]
	, [Cat "like" "V" [Infl] [Cat "_" "NP" [AccOrDat] []] ]
	, [Cat "lived"	"V" [Tense] [Cat "_" "PP" [In] []] ]
	, [Cat "live"	"V" [Infl] [Cat "_" "PP" [In] []] ]
	]

aux = [
	]

adjs = [
	[Cat "shy"	"ADJ" [] []]
	, [Cat "reserved"	"ADJ" [] []]
	, [Cat "good"	"ADJ" [] []]
	, [Cat "nice"	"ADJ" [] []]
	, [Cat "busy"	"ADJ" [] []]

	]

advs = [
	]
