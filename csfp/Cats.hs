module Cats where

import Parsing

class_names, prons, reflexives, interrogatives, aux, intransitives, transitives, ditransitives, determiners, preps, conjuncts :: Lexset

class_names = [
--noun and verb
	[Cat "parent" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "parent" "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "parents" "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "father"    "CN" [Sg,Masc,Thrd] []],
	[Cat "mother"    "CN" [Sg,Fem,Thrd] []],
	[Cat "daughter"    "CN" [Sg,Fem,Thrd] []],
	[Cat "daughters"    "CN" [Pl,Fem,Thrd] []],
	[Cat "son"    "CN" [Sg,Masc,Thrd] []],
	[Cat "sons"    "CN" [Pl,Masc,Thrd] []],
	[Cat "brother"    "CN" [Sg,Masc,Thrd] []],
	[Cat "husband"	"CN" [Thrd,Masc,Sg] []],
	[Cat "wife"   "CN" [Sg,Fem,Thrd]   []],
	[Cat "thing"   "CN" [Sg,Neutr,Thrd] []],
	[Cat "things"  "CN" [Pl,Neutr,Thrd] []],
	[Cat "person"  "CN" [Sg,Masc,Thrd]  []],
	[Cat "persons" "CN" [Pl,Masc,Thrd]  []],
	[Cat "boy"     "CN" [Sg,Masc,Thrd]  []],
	[Cat "boys"    "CN" [Pl,Masc,Thrd]  []],
	[Cat "man"     "CN" [Sg,Masc,Thrd]  []],
	[Cat "men"     "CN" [Pl,Masc,Thrd]  []],
	[Cat "girl"    "CN" [Sg,Fem,Thrd]   []],
	[Cat "girls"   "CN" [Pl,Fem,Thrd]   []],
	[Cat "woman"   "CN" [Sg,Fem,Thrd]   []],
	[Cat "women"   "CN" [Pl,Fem,Thrd]   []],
	[Cat "supervisor" "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "supervisors" "CN" [Pl,MascOrFem,Thrd]   []]
	]

possessives = [
	[Cat "'s" "APOS" [] []],
	[Cat "of" "OFPOS" [] []]
	]

prons = [
	[Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []],
	[Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []],
	[Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []],
	[Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []],
	[Cat "you" "NP" [Pers,Snd]               []],
	[Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []],
	[Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] []],
	[Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []],
	[Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] []],
	[Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []],
	[Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []],
	[Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] []]
	]

reflexives = [
	[Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []],
	[Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []],
	[Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []],
	[Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []],
	[Cat "himself"   "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []],
	[Cat "herself"   "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []],
	[Cat "itself"    "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []],
	[Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]
	]

interrogatives = [
	[Cat "who" "NP"  [Wh,Thrd,MascOrFem] [],
			Cat "who" "REL" [MascOrFem]         []],
	[Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [],
			Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []],
	[Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []],
	[Cat "that"  "REL" [] [], Cat "that"  "DET" [Sg]    []],
	[Cat "which" "REL" [Neutr] [], Cat "which" "DET" [Wh] []],
	[Cat "where" "NP" [Neutr] [], Cat "where" "REL" [] []]
	]

determiners = [
	[Cat "zero"     "DET" [Pl]  []],
	[Cat "every"   "DET" [Sg]  []],
	[Cat "all"     "DET" [Pl]  []],
	[Cat "some"    "DET" []    []],
	[Cat "several" "DET" [Pl]  []],
	[Cat "a"       "DET" [Sg]  []],
	[Cat "no"      "DET" []    []],
	[Cat "the"     "DET" []    []],
	[Cat "most"    "DET" [Pl]  []],
	[Cat "many"    "DET" [Pl]  []],
	[Cat "few"     "DET" [Pl]  []],
	[Cat "this"    "DET" [Sg]  []],
	[Cat "these"   "DET" [Pl]  []],
	[Cat "those"   "DET" [Pl]  []],
	[Cat "less_than" "DF" [Pl] []],
	[Cat "more_than" "DF" [Pl] []]
	]

aux = [
	[Cat "did"    "AUX" [] []],
	[Cat "didn't" "AUX" [] []]
	]

cops = [
	[Cat "was"    "COP" [Sg] []],
	[Cat "were"   "COP" [Pl] []]
	]

gerunds = [
	[Cat "ing"    "GER" [] []]
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
	[Cat "disappointed"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
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
--	[Cat "spoke"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
--	[Cat "speak"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
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
	[Cat "raised" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	[Cat "raise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

ditransitives = [
	[Cat "studied" "VP" [Tense] [Cat "_" "PP" [At] []],
		Cat "studied" "VP" [Tense] [Cat "_" "PP" [In] []],
		Cat "studied" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "studied" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [At] []],
		Cat "studied" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [In] []]],
	[Cat "study" "VP" [Infl] [Cat "_" "PP" [At] []],
		Cat "study" "VP" [Infl] [Cat "_" "PP" [In] []],
		Cat "study" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "study" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [At] []],
		Cat "study" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [In] []]],
	[Cat "worked" "VP" [Tense] [],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [As] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [At,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [On,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [In,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [For,Neutr] []]],
	[Cat "work" "NP" [Sg,Neutr,Thrd]  [],
		Cat "work" "CN" [Sg,Neutr,Thrd] [],
		Cat "work" "VP" [Infl] [],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [As,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [At,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [On,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [In,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [For,Neutr] []]],
	[Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat]  []]],
	[Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]],
	[Cat "got" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
			Cat "got" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From]  []]],
	[Cat "get" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
			Cat "get" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "accepted" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
			Cat "accepted" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From]  []]],
	[Cat "accept" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
			Cat "accept" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [From] []]],
	[Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] []],
			Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]],
	[Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
			Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]
	]

preps = [
	[Cat "about"   "PREP" [About]   []],
	[Cat "as"   "PREP" [As]   []],
	[Cat "at"   "PREP" [At]   []],
	[Cat "by"   "PREP" [By]   []],
	[Cat "for"  "PREP" [For]  []],
	[Cat "from" "PREP" [From] []],
	[Cat "in"   "PREP" [In]   []],
	[Cat "on"   "PREP" [On]   []],
	[Cat "to"   "PREP" [To]   []],
	[Cat "through" "PREP" [Through] []],
	[Cat "with" "PREP" [With] []]
	]

conjuncts = [
	-- [Cat "and"  "CONJ" [] []],
	[Cat "but"  "CONJ" [] []],
	[Cat "."    "CONJ" [] []]
	-- [Cat "if"   "COND" [] []],
	-- [Cat "then" "THEN" [] []]
	]


