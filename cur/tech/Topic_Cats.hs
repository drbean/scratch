module Topic_Cats where

import Parsing

nouns, intransitives, transitives :: Lexset

nouns = [
--noun and verb
	[Cat "thing"   "CN" [Sg,Neutr,Thrd] []],
	[Cat "things"  "CN" [Pl,Neutr,Thrd] []],
	[Cat "person"  "CN" [Sg,Masc,Thrd]  []],
	[Cat "persons" "CN" [Pl,Masc,Thrd]  []],
	-- [Cat "boy"     "CN" [Sg,Masc,Thrd]  []],
	-- [Cat "boys"    "CN" [Pl,Masc,Thrd]  []],
	-- [Cat "man"     "CN" [Sg,Masc,Thrd]  []],
	-- [Cat "men"     "CN" [Pl,Masc,Thrd]  []],
	-- [Cat "child"    "CN" [Sg,MascOrFem,Thrd]   []],
	-- [Cat "children"   "CN" [Pl,MascOrFem,Thrd]   []],
	-- [Cat "girl"    "CN" [Sg,Fem,Thrd]   []],
	-- [Cat "girls"   "CN" [Pl,Fem,Thrd]   []],

	[Cat "company"   "CN" [Sg,Neutr,Thrd]   []],
	[Cat "manager"   "CN" [Sg,MascOrFem,Thrd]   []],
	-- [Cat "worker"   "CN" [Sg,MascOrFem,Thrd]   []],
	-- [Cat "workers"   "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "administrative_assistant"   "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "administrative_assistants"   "CN" [Pl,MascOrFem,Thrd]   []]
	]

intransitives = [
	]

transitives = [
	--[Cat "say"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "said"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [About] []],
	--	Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "NP" [AccOrDat] []]],
	--[Cat "ask"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [About] []],
	--	Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "NP" [AccOrDat] []]],
	--[Cat "spoke"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "speak"	"V" [Infl] [Cat "_" "PP" [To] [],
	--				Cat "_" "PP" [About] []]],
	--[Cat "talked"	"V" [Tense] [Cat "_" "PP" [To] []],
	--	Cat "talked"	"V" [Tense] [Cat "_" "PP" [About] []],
	--	Cat "talked"	"V" [Tense] [Cat "_" "PP" [To] [],
	--				Cat "_" "PP" [About] []]],
	--[Cat "talk"	"V" [Infl]  [Cat "_" "PP" [To] []],
	--	Cat "talk"	"V" [Infl] [Cat "_" "PP" [About] []],
	--	Cat "talk"	"V" [Infl] [Cat "_" "PP" [About] [],
	--				Cat "_" "PP" [With] []],
	--	Cat "talk"	"V" [Infl] [Cat "_" "PP" [To] [],
	--				Cat "_" "PP" [About] []]],
	--[Cat "knew"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "knew"	"V" [Tense] [Cat "_" "PP" [About] []]],
	--[Cat "know"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "know"	"V" [Infl] [Cat "_" "PP" [About] []]],
	--[Cat "left"	"V" [Tense] [],
	--	Cat "left"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "left"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [For] []]],
	--[Cat "leave"	"V" [Infl]  [],
	--	Cat "leave"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "left"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [For] []]],
	--[Cat "went"	"V" [Tense] [Cat "_" "PP" [To] []]],
	--[Cat "go"	"V" [Infl]  [Cat "_" "PP" [To] []]],
	--[Cat "came"	"V" [Tense] [Cat "_" "PP" [From] []]],
	--[Cat "come"	"V" [Infl]  [Cat "_" "PP" [From] []]]
	]

