module Topic_Cats where

import Parsing

nouns, intransitives, transitives :: Lexset

nouns = [
--noun and verb
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
	]

transitives = [
	[Cat "say"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "said"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
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
	]

