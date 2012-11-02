module Story_Cats where

import Parsing

names, nouns, verbs, aux, adjs, advs :: Lexset

names = [
	[Cat "one_month" "NP" [Thrd,Neutr,Sg] []],
	[Cat "two_months" "NP" [Thrd,Neutr,Sg] []],
	[Cat "6_000_dollars" "NP" [Thrd,Neutr,Sg] []],
	[Cat "3_000_dollars" "NP" [Thrd,Neutr,Sg] []],
	[Cat "50_dollars_an_hour" "NP" [Thrd,Neutr,Sg] []],
	[Cat "michelle"	"NP" [Thrd,Fem,Sg] []],
	[Cat "larry"	"NP" [Thrd,Masc,Sg] []]
	]

nouns = [

	-- [Cat "area"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "time"	"CN" [Thrd,Neutr,Sg] [],
	-- 	Cat "time"	"NP" [Thrd,Neutr,Sg] []],
	-- [Cat "dollars"	"CN" [Thrd,Neutr,Pl] []],
	-- [Cat "money"	"CN" [Thrd,Neutr,Sg] [],
	-- 	Cat "money"	"NP" [Thrd,Neutr,Sg] []],
	-- [Cat "payment"	"CN" [Thrd,Neutr,Sg] [],
	-- 	Cat "payment"	"NP" [Thrd,Neutr,Sg] []],
	-- [Cat "design"	"CN" [Thrd,Neutr,Sg] [],
	-- 	Cat "design"	"NP" [Thrd,Neutr,Sg] []],
	-- [Cat "deal"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "bookstore"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "image"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "images"	"CN" [Thrd,Neutr,Pl] []],
	-- [Cat "books"	"CN" [Thrd,Neutr,Pl] []],
	[Cat "website"	"CN" [Thrd,Neutr,Sg] []],
	[Cat "site"	"CN" [Thrd,Neutr,Sg] []]
	-- [Cat "size"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "amount"	"CN" [Thrd,Neutr,Sg] []],
	-- [Cat "information"	"NP" [Thrd,Neutr,Sg] []],

	-- [Cat "agreement"	"NP" [Thrd,Neutr,Sg] []],

	-- [Cat "manager"	"CN" [Thrd,MascOrFem,Sg] []],
	-- [Cat "website_designer"	"CN" [Thrd,MascOrFem,Sg] []]
	]

verbs = [
	-- [Cat "liked" "VP" [Tense] [Cat "_" "NP" [AccOrDat]       []]],
	-- [Cat "like" "VP" [Infl] [Cat "_" "NP" [AccOrDat]       []]],
	-- [Cat "accepted" "VP" [Tense] [Cat "_" "NP" [AccOrDat]       []]],
	-- [Cat "accept" "VP" [Infl] [Cat "_" "NP" [AccOrDat]       []]],
	-- [Cat "agreed" "VP" [Tense] [Cat "_" "INF" [To]       []]],
	-- [Cat "agree" "VP" [Infl] [Cat "_" "INF" [To]       []]],
        -- [Cat "compromised" "VP" [Tense] [Cat "_" "PP" [On]       []]],
        -- [Cat "compromise" "VP" [Infl] [Cat "_" "PP" [On]       []]],
        -- [Cat "came" "VP" [Tense] [Cat "_" "PP" [To]       []]],
        -- [Cat "come" "VP" [Infl] [Cat "_" "PP" [To]       []]],
        -- [Cat "operate" "VP" [Infl] [Cat "_" "PP" [In]       []]],
	-- [Cat "show"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
	-- 	Cat "show"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
	-- 					Cat "_" "PP" [On] []]],
	-- [Cat "put"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
	-- 				Cat "_" "PP" [On] []]],
	-- [Cat "take"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
	-- 	Cat "take"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
	-- 					Cat "_" "INF" [To] []]],
	[Cat "finish"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [In] []]],
	-- [Cat "limit"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []]],

	[Cat "pay"	"VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "pay" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []]],
        [Cat "charge" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
		Cat "charge" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                                                Cat "_" "NP" [AccOrDat] []]]

	]

aux = [
	]

adjs = [
	-- [Cat "ready"	"ADJ" [] [Cat "_" "INF" [To]       []]],
	-- [Cat "lots_of"	"ADJ" [] []],
	-- [Cat "a_lot_of"	"ADJ" [] []],
	-- [Cat "popular"	"ADJ" [] []],
	-- [Cat "good"	"ADJ" [] []]
	]

advs = [
	-- [Cat "slowly"	"ADV" [] []]
	]
