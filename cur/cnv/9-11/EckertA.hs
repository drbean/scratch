module Eckert where

import PP

lexicon :: String -> [Cat]

lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] 
					 []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] 
					 []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] 
					 []]

lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]

lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]

lexicon "beverly"        = woman "beverly"
lexicon "marvin"      = man "marvin"
lexicon "sean"   = man "sean"
lexicon "mohamed"       = man "mohamed"

lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]
lexicon "at_least"  = [Cat "at_least"  "DF" [Pl] []]
lexicon "at_most"   = [Cat "at_most"   "DF" [Pl] []]
lexicon "exactly"   = [Cat "exactly"   "DF" [Pl] []]

lexicon "0" = [Cat "0" "DIG" [Pl] []]
lexicon "1" = [Cat "1" "DIG" [Sg] []]
lexicon "2" = [Cat "2" "DIG" [Pl] []]
lexicon "3" = [Cat "3" "DIG" [Pl] []]
lexicon "4" = [Cat "4" "DIG" [Pl] []]
lexicon "5" = [Cat "5" "DIG" [Pl] []]
lexicon "6" = [Cat "6" "DIG" [Pl] []]
lexicon "7" = [Cat "7" "DIG" [Pl] []]
lexicon "8" = [Cat "8" "DIG" [Pl] []]
lexicon "9" = [Cat "9" "DIG" [Pl] []]

lexicon "place"   = [Cat "place"   "CN" [Sg,Neutr,Thrd] []]
lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd]   []]

lexicon "smoke"    = [Cat "smoke"    "UN" [Sg,Neutr,Thrd] []]
lexicon "high_school_dance"    = thing "high_school_dance"

lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"    = [Cat "smiled"    "VP" [Tense] []]
lexicon "smile"     = [Cat "smile"     "VP" [Infl]  []]
lexicon "laughed"   = [Cat "laughed"   "VP" [Tense] []]
lexicon "laugh"     = [Cat "laugh"     "VP" [Infl]  []]
lexicon "breathed"   = [Cat "breathed"   "VP" [Tense] []]
lexicon "breathe"     = [Cat "breathe"     "VP" [Infl]  []]
lexicon "died" = smiled "died"
lexicon "die" = smile "die"

lexicon "heard"        = 
 [Cat "heard"    "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "hear"         = 
 [Cat "hear"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "loved"        = 
 [Cat "loved"    "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         = 
 [Cat "love"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "admired"      = 
 [Cat "admired"  "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       = 
 [Cat "admire"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped"       = 
 [Cat "helped"   "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"         = 
 [Cat "help"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeated"       = 
 [Cat "defeated" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"         = 
 [Cat "defeat"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
	                   Cat "_" "PP" [To]       []], 
  Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
	                   Cat "_" "NP" [AccOrDat]  []]]
lexicon "give"         = 
 [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sold" = 
 [Cat "sold" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sold" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = 
 [Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "met" = met "met"
lexicon "meet" = meet "meet"
lexicon "paused" = met "paused"
lexicon "pause" = meet "pause"

lexicon "kicked" = 
 [Cat "kicked" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick" = 
 [Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	                     Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "took" = 
 [Cat "took" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
	                   Cat "_" "PP" [From]     []], 
  Cat "took" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
lexicon "take" = 
 [Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "about"   = prep "About"
lexicon "into"   = prep "Into"
lexicon "in"   = prep "In"
lexicon "at"   = prep "At"
lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]
lexicon "but"  = conj "but"

lexicon _ = []

testSuite1 :: [String]
testSuite1 = 
 [
   "Who did Sean love?",
   "Who did Mohamed love?",
   "What did Beverly hear?",
   "Who did Sean meet at a high school dance?",
   "In which place did Beverly and Sean meet?",
   "Where did Beverly and Sean meet?"

   ]

testSuite2 :: [String]
testSuite2 = 
 [
   "Sean was on the 105th floor trying to find a way out, but the stairwell was full of smoke."
   ]

