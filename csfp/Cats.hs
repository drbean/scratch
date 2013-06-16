module Cats where

import Parsing

class_names, prons, reflexives, interrogatives, aux, cops, gerunds, transitives, ditransitives, determiners, preps, advs, conjuncts :: Lexset -- intransitives, 

class_names = [
--noun and verb
	[Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
	, [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
	, [Cat "person"  "CN" [Sg,MascOrFem,Thrd]  []]
	, [Cat "persons" "CN" [Pl,MascOrFem,Thrd]  []]
	, [Cat "people" "CN" [Pl,MascOrFem,Thrd]  []]
	-- , [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
	-- , [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
	-- , [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
	-- , [Cat "women"   "CN" [Pl,Fem,Thrd]   []]
	]

possessives = [
	[Cat "'s" "APOS" [] []],
	[Cat "of" "OFPOS" [] []]
	]

prons = [
	-- [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []],
	-- [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []],
	-- [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []],
	-- [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []],
	-- [Cat "you" "NP" [Pers,Snd]               []],
	--[Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []],
	---- [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] []],
	--[Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []],
	---- [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] []],
	--[Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []],
	--[Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
	---- [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] []]
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
	[Cat "who" "NP"  [Wh,Thrd,MascOrFem] []
			, Cat "who" "REL" [MascOrFem]         []]
	--, [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] []
	--		, Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
	, [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    [], Cat "what" "DET" [Wh] []]
	--, [Cat "that"  "REL" [] [], Cat "that"  "DET" [Sg]    []]
	, [Cat "which" "REL" [Neutr] [], Cat "which" "DET" [Wh] []]
	, [Cat "whose" "REL" [Neutr] [], Cat "whose" "DET" [Wh] []]
	---- [Cat "how_much" "NP" [Wh,Thrd,Neutr] [], Cat "how_much" "DET" [Wh] []]
	, [Cat "where" "NP" [Wh,Neutr] [], Cat "where" "REL" [Neutr] []]
	, [Cat "when" "NP" [Wh,Neutr] [], Cat "when" "REL" [Neutr] []]
	]

interrolist = map (phon . head) interrogatives

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
	[Cat "few"     "DET" [Pl]  []]
	-- [Cat "this"    "DET" [Sg]  []],
	-- [Cat "these"   "DET" [Pl]  []],
	-- [Cat "those"   "DET" [Pl]  []],
	-- [Cat "less_than" "DET" [] []],
	-- [Cat "more_than" "DET" [] []]
	]

aux = [
	[Cat "did"    "AUX" [Pos] [], Cat "did"    "TAG" [Pos] []],
	[Cat "didn't" "AUX" [Ng] [], Cat "didn't" "TAG" [Ng] []]
	]
cops = [
	[Cat "was"     "AUX" [Pos,Sg] [],
	 Cat "was"     "COP" [Pos,Sg] [],
	 Cat "was"     "COP" [Pos,Sg] [Cat "_" "NP" [] []],
	 Cat "was"     "COP" [Pos,Sg] [Cat "_" "ADJ" [] []],
	 Cat "was"     "COP" [Pos,Sg] [Cat "_" "PP" [BecauseOf] []]],
	[Cat "wasn't"  "COP" [Ng,Sg] [],
	 Cat "wasn't"  "COP" [Ng,Sg] [Cat "_" "NP" [] []],
	 Cat "wasn't"  "COP" [Ng,Sg] [Cat "_" "ADJ" [] []],
	 Cat "wasn't"  "COP" [Ng,Sg] [Cat "_" "PP" [] []]],
	[Cat "were"    "COP" [Pos,Pl] [],
	 Cat "were"    "COP" [Pos,Pl] [Cat "_" "NP" [] []],
	 Cat "were"    "COP" [Pos,Pl] [Cat "_" "ADJ" [] []],
	 Cat "were"    "COP" [Pos,Pl] [Cat "_" "PP" [] []]],
	[Cat "weren't" "COP" [Ng,Pl] [],
	 Cat "weren't" "COP" [Ng,Pl] [Cat "_" "NP" [] []],
	 Cat "weren't" "COP" [Ng,Pl] [Cat "_" "ADJ" [] []],
	 Cat "weren't" "COP" [Ng,Pl] [Cat "_" "PP" [] []]]
	]

gerunds = [
	[Cat "ing"    "GER" [] []]
	]

intransitives = [
	--[Cat "separated"    "V" [Tense] []],
	--[Cat "separate"     "V" [Infl]  []],
	--[Cat "got_married"    "V" [Tense] [],
	--	Cat "got_married"	"V" [Tense] [Cat "_" "PP" [In] []]],
	--[Cat "get_married"     "V" [Infl]  [],
	--	Cat "get_married"	"V" [Infl] [Cat "_" "PP" [In] []]],
	--[Cat "got_divorced"    "V" [Tense] [],
	--	Cat "got_divorced"	"V" [Tense] [Cat "_" "PP" [In] []]],
	--[Cat "get_divorced"     "V" [Infl]  [],
	--	Cat "get_divorced"	"V" [Infl] [Cat "_" "PP" [In] []]]
	]

transitives = [
	[Cat "had" "V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	, [Cat "have" "V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	--, [Cat "wanted" "V" [Tense] [Cat "_" "INF" [To] []],
	--	Cat "wanted" "V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "wanted" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From] []],
	--	Cat "wanted" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "INF" [To] []]]
	--, [Cat "want" "V" [Infl] [Cat "_" "INF" [To] []],
	--	Cat "want" "V" [Infl] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "want" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From] []],
	--	Cat "want" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "INF" [To] []]]
	--, [Cat "decided" "V" [Tense] [Cat "_" "INF" [To] []],
	--	Cat "decided" "V" [Tense] [Cat "_" "NP" [AccOrDat] []]]
	--, [Cat "decide" "V" [Infl] [Cat "_" "INF" [To] []],
	--	Cat "decide" "V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	--[Cat "appreciated"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "appreciate"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "disappointed"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "disappoint"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "looked_back"	"V" [Tense] [],
	--	Cat "looked_back"	"V" [Tense] [Cat "_" "PP" [On] []]],
	--[Cat "look_back"	"V" [Infl] [],
	--	Cat "look_back"	"V" [Infl] [Cat "_" "PP" [On] []]],
	--[Cat "saw"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "see"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "say"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "said"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "married"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "married"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [In] []]],
	--[Cat "marry"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "marry"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [In] []]],
	--[Cat "divorced"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "divorce"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "asked"	"V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [About] []]],
	--[Cat "ask"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "ask"	"V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [About] []]],
	--[Cat "spoke"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "speak"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "knew"	"V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "knew"	"V" [Tense] [Cat "_" "PP" [About] []]],
	--[Cat "know"	"V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--	Cat "knew"	"V" [Tense] [Cat "_" "PP" [About] []]],
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
	--[Cat "come"	"V" [Infl]  [Cat "_" "PP" [From] []]],
	--[Cat "raised" "V" [Tense] [Cat "_" "NP" [AccOrDat] []]],
	--[Cat "raise" "V" [Infl] [Cat "_" "NP" [AccOrDat] []]]
	]

ditransitives = [
	--[Cat "studied" "V" [Tense] [Cat "_" "PP" [At] []],
	--	Cat "studied" "V" [Tense] [Cat "_" "PP" [In] []],
	--	Cat "studied" "V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "studied" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [At] []],
	--	Cat "studied" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [In] []]],
	--[Cat "study" "V" [Infl] [Cat "_" "PP" [At] []],
	--	Cat "study" "V" [Infl] [Cat "_" "PP" [In] []],
	--	Cat "study" "V" [Infl] [Cat "_" "NP" [AccOrDat] []],
	--	Cat "study" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [At] []],
	--	Cat "study" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--				Cat "_" "PP" [In] []]],
	--[Cat "worked" "V" [Tense] [],
	--	Cat "worked" "V" [Tense] [Cat "_" "PP" [As] []],
	--	Cat "worked" "V" [Tense] [Cat "_" "PP" [At,Neutr] []],
	--	Cat "worked" "V" [Tense] [Cat "_" "PP" [On,Neutr] []],
	--	Cat "worked" "V" [Tense] [Cat "_" "PP" [In,Neutr] []],
	--	Cat "worked" "V" [Tense] [Cat "_" "PP" [For,Neutr] []]],
	--[Cat "work" "NP" [Sg,Neutr,Thrd]  [],
	--	Cat "work" "NP" [Sg,Neutr,Thrd] [],
	--	Cat "work" "V" [Infl] [],
	--	Cat "work" "V" [Infl] [Cat "_" "PP" [As,Neutr] []],
	--	Cat "work" "V" [Infl] [Cat "_" "PP" [At,Neutr] []],
	--	Cat "work" "V" [Infl] [Cat "_" "PP" [On,Neutr] []],
	--	Cat "work" "V" [Infl] [Cat "_" "PP" [In,Neutr] []],
	--	Cat "work" "V" [Infl] [Cat "_" "PP" [For,Neutr] []]],
	--[Cat "gave" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [To]       []],
	--		Cat "gave" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "NP" [AccOrDat]  []]],
	--[Cat "give" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [To]       []],
	--		Cat "give" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "NP" [AccOrDat] []]],
	--[Cat "got" "V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--		Cat "got" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  []]],
	--[Cat "get" "V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--		Cat "get" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From] []]],
	--[Cat "took" "V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--		Cat "took" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [For]  []],
	--		Cat "took" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  []],
	--		Cat "took" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  [],
	--					Cat "_" "PP" [For]  []]],
	--[Cat "take" "V" [Infl] [Cat "_" "NP" [AccOrDat] []],
	--		Cat "take" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [For]  []],
	--		Cat "take" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  []],
	--		Cat "take" "V" [Infl] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  [],
	--					Cat "_" "PP" [For]  []]]
	--[Cat "accepted" "V" [Tense] [Cat "_" "NP" [AccOrDat] []],
	--		Cat "accepted" "V" [Tense] [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From]  []]],
	--[Cat "accept" "V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--		Cat "accept" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [From] []]],
	--[Cat "told" "V" [Tense]  [Cat "_" "NP" [AccOrDat] []],
	--		Cat "told" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [To]       []],
	--		Cat "told" "V" [Tense]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "NP" [AccOrDat] []]],
	--[Cat "tell" "V" [Infl]  [Cat "_" "NP" [AccOrDat] []],
	--		Cat "tell" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "PP" [To]       []],
	--		Cat "tell" "V" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	--					Cat "_" "NP" [AccOrDat] []]]
	]

preps = [
	[Cat "in"   "PREP" [In]   []]
	, [Cat "by"   "PREP" [By]   []]
	--, [Cat "around"   "PREP" [Around]   []]
	--, [Cat "about"   "PREP" [About]   []]
	--, [Cat "after"   "PREP" [After]   []]
	--, [Cat "as"   "PREP" [As]   []]
	--, [Cat "at"   "PREP" [At]   []]
	--, [Cat "because_of"   "PREP" [BecauseOf]   []]
	--, [Cat "for"  "PREP" [For]  []]
	--, [Cat "from" "PREP" [From] []]
	--, -- [Cat "like" "PREP" [Like] []]
	--, [Cat "on"   "PREP" [On]   []]
	--, [Cat "to"   "PREP" [To]   []
	--, 	-- Cat "to" "TO" [ToInf] [Cat "_" "VP" [] []]]
	--, 	Cat "to" "TO" [ToInf] []]
	--, [Cat "through" "PREP" [Through] []]
	--, [Cat "with" "PREP" [With] []]
	]

advs = [
	]

conjuncts = [
	-- [Cat "and"  "CONJ" [] []],
	[Cat "but"  "CONJ" [] []],
	[Cat "."    "CONJ" [] []]
	-- [Cat "if"   "COND" [] []],
	-- [Cat "then" "THEN" [] []]
	]


