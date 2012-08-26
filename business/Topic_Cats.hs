module Topic_Cats where

import Parsing

nouns, intransitives, transitives :: Lexset

nouns = [
--noun and verb
	[Cat "parent" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "parent" "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "parents" "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "father"    "CN" [Sg,Masc,Thrd] []],
	[Cat "mother"    "CN" [Sg,Fem,Thrd] []],
	[Cat "daughter"    "CN" [Sg,Fem,Thrd] []],
	[Cat "son"    "CN" [Sg,Masc,Thrd] []],
	[Cat "sons"    "CN" [Pl,Masc,Thrd] []],
	[Cat "brother"    "CN" [Sg,Masc,Thrd] []],
	[Cat "sister"    "CN" [Sg,Fem,Thrd] []],
	[Cat "husband"	"CN" [Thrd,Masc,Sg] []],
	[Cat "thing"   "CN" [Sg,Neutr,Thrd] []],
	[Cat "things"  "CN" [Pl,Neutr,Thrd] []],
	[Cat "person"  "CN" [Sg,Masc,Thrd]  []],
	[Cat "persons" "CN" [Pl,Masc,Thrd]  []],
	[Cat "boy"     "CN" [Sg,Masc,Thrd]  []],
	[Cat "boys"    "CN" [Pl,Masc,Thrd]  []],
	[Cat "man"     "CN" [Sg,Masc,Thrd]  []],
	[Cat "men"     "CN" [Pl,Masc,Thrd]  []],
	[Cat "child"    "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "children"   "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "girl"    "CN" [Sg,Fem,Thrd]   []],
	[Cat "girls"   "CN" [Pl,Fem,Thrd]   []],
	[Cat "woman"   "CN" [Sg,Fem,Thrd]   []],
	[Cat "women"   "CN" [Pl,Fem,Thrd]   []],

	[Cat "company"   "CN" [Sg,Neutr,Thrd]   []],
	[Cat "worker"   "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "workers"   "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "administrative_assistant"   "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "administrative_assistants"   "CN" [Pl,MascOrFem,Thrd]   []]
	]

intransitives = [
	[Cat "separated"    "VP" [Tense] []],
	[Cat "separate"     "VP" [Infl]  []],
	[Cat "got_married"    "VP" [Tense] [],
		Cat "got_married"	"VP" [Tense] [Cat "_" "PP" [In] []]],
	[Cat "get_married"     "VP" [Infl]  [],
		Cat "get_married"	"VP" [Infl] [Cat "_" "PP" [In] []]],
	[Cat "got_divorced"    "VP" [Tense] [],
		Cat "got_divorced"	"VP" [Tense] [Cat "_" "PP" [In] []]],
	[Cat "get_divorced"     "VP" [Infl]  [],
		Cat "get_divorced"	"VP" [Infl] [Cat "_" "PP" [In] []]]
	]

transitives = [
	[Cat "appreciated"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "appreciate"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "disappointed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "disappointed"	"ADJ" [] []],
	[Cat "disappoint"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "looked_back"	"VP" [Tense] [],
		Cat "looked_back"	"VP" [Tense] [Cat "_" "PP" [On] []]],
	[Cat "look_back"	"VP" [Infl] [],
		Cat "look_back"	"VP" [Infl] [Cat "_" "PP" [On] []]],
	[Cat "saw"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "see"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "say"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "said"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "married"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "married"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [In] []]],
	[Cat "marry"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "marry"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [In] []]],
	[Cat "divorced"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "divorce"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "asked"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "asked"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [About] []]],
	[Cat "ask"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "ask"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [About] []]],
	[Cat "spoke"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "speak"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "talked"	"VP" [Tense] [Cat "_" "PP" [To] []],
--		Cat "talked"	"VP" [Tense] [Cat "_" "PP" [About] []],
		Cat "talked"	"VP" [Tense] [Cat "_" "PP" [To] [],
					Cat "_" "PP" [About] []]],
	[Cat "talk"	"VP" [Infl]  [Cat "_" "PP" [To] []],
--		Cat "talk"	"VP" [Infl] [Cat "_" "PP" [About] []],
		Cat "talk"	"VP" [Infl] [Cat "_" "PP" [To] [],
					Cat "_" "PP" [About] []]],
	[Cat "knew"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "knew"	"VP" [Tense] [Cat "_" "PP" [About] []]],
	[Cat "know"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "knew"	"VP" [Tense] [Cat "_" "PP" [About] []]],
	[Cat "had"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "have"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	[Cat "left"	"VP" [Tense] [],
		Cat "left"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "left"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [For] []]],
	[Cat "leave"	"VP" [Infl]  [],
		Cat "leave"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "left"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [For] []]],
	[Cat "went"	"VP" [Tense] [Cat "_" "PP" [To] []]],
	[Cat "go"	"VP" [Infl]  [Cat "_" "PP" [To] []]],
	[Cat "came"	"VP" [Tense] [Cat "_" "PP" [From] []]],
	[Cat "come"	"VP" [Infl]  [Cat "_" "PP" [From] []]],
	[Cat "immigrated"	"VP" [Tense] [],
		Cat "immigrated"	"VP" [Tense] [Cat "_" "PP" [To] []],
		Cat "immigrated" "VP" [Tense] [Cat "_" "PP" [From] []]],
	[Cat "immigrate"	"VP" [Infl] [],
		Cat "immigrate"	"VP" [Infl] [Cat "_" "PP" [To] []],
		Cat "immigrate" "VP" [Infl] [Cat "_" "PP" [From] []]],
	[Cat "raised" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "raise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

