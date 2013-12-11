module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "alex"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "dave"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "jeff"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "neil"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "shane"	"NP" [Thrd,Masc,Sg] []]
	-- , [Cat "cindy"	"NP" [Thrd,Fem,Sg] []]
	-- , [Cat "kelly"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "avril_lavigne"	"NP" [Thrd,Fem,Sg] []]
	-- , [Cat "mindy"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "rena"	"NP" [Thrd,Fem,Sg] []]
	-- , [Cat "vicky"	"NP" [Thrd,Fem,Sg] []]
	, [Cat "applied_foreign_languages"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "minghsin_university"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "hello_kitty"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "the_color_pink"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "mi_mi"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "america"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "taiwan"	"NP" [Thrd,Neutr,Sg] []]
	-- , [Cat "hsinchu"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "music"	"NP" [Thrd,Neutr,Sg] []]

	]

nouns = [
	[Cat "teacher"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "student"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "students"	"CN" [Thrd,MascOrFem,Pl] []]


	]

verbs = [
	[Cat "born"	"V" [Part] []
	, Cat "born"	"V" [Part] [Cat "_" "PP" [In] []]
	, Cat "born"	"V" [Part] [Cat "_" "PP" [In] [], Cat "_" "PP" [In] []]]
	--, [Cat "started"	"V" [Part] []
	--, Cat "started"	"V" [Part] [Cat "_" "PP" [In] []]
	--, Cat "started"	"V" [Part] [Cat "_" "PP" [By] [], Cat "_" "PP" [In] []]
	--, Cat "started"	"V" [Part] [Cat "_" "PP" [In] [], Cat "_" "PP" [By] []]
	--, Cat "started" "V" [Tense] [Cat "_" "NP" [AccOrDat] []]
	--, Cat "started" "V" [Tense] [Cat "_" "NP" [AccOrDat] []
	--			, Cat "_" "PP" [In] []]]
	--, [Cat "start" "V" [Infl] [Cat "_" "NP" [AccOrDat] []]
	--, Cat "start" "V" [Infl] [Cat "_" "NP" [AccOrDat] []
	--			, Cat "_" "PP" [In] []]]
	]

aux = [
	]

adjs = [
	]

advs = [
	]
