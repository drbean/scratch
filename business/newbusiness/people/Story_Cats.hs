module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "dr_bean"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "morris_chang"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "jensen_huang"	"NP" [Thrd,Masc,Sg] []]
	, [Cat "stanford_university"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "mit"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "china"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "taiwan"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "nvidia"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "tsmc"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "1931"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "1963"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "1993"	"NP" [Thrd,Neutr,Sg] []]

	]

nouns = [
	[Cat "teacher"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "ceo"	"CN" [Thrd,MascOrFem,Sg] []]
	, [Cat "ceos"	"CN" [Thrd,MascOrFem,Pl] []]
	, [Cat "electrical_engineering"	"NP" [Thrd,Neutr,Sg] []]
	, [Cat "mechanical_engineering"	"NP" [Thrd,Neutr,Pl] []]

	, [Cat "master's_degree"	"CN" [Thrd,Neutr,Sg] []]
	, [Cat "phd_degree"	"CN" [Thrd,Neutr,Sg] []]


	]

verbs = [
	[Cat "born"	"V" [Part] []
	 , Cat "born"	"V" [Part] [Cat "_" "PP" [In] []]]
	, [Cat "started"	"V" [Part] []
	 , Cat "started"	"V" [Part] [Cat "_" "PP" [In] []]
	 , Cat "started" "V" [Tense] [Cat "_" "NP" [AccOrDat] []]
	 , Cat "started" "V" [Tense] [Cat "_" "NP" [AccOrDat] []
	 				, Cat "_" "NP" [In] []]]
	]

aux = [
	]

adjs = [
	]

advs = [
	]
