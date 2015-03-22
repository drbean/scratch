module EckertB where

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
lexicon "sean"   = man "sean"

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

lexicon "smoke"    = [Cat "smoke"    "CN" [Sg,Neutr,Thrd] []]
lexicon "happiness"    = [Cat "happiness"    "CN" [Sg,Neutr,Thrd] []]
lexicon "high_school_dance"    = thing "high_school_dance"
lexicon "phone" = thing "phone"
lexicon "lie" = thing "lie"

-- lexicon "was"    = [Cat "was"    "AUX" [] []]
lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "paused" = smile Tense "paused"
lexicon "pause" = smile Infl "pause"

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

lexicon "talked" = talk Tense "talked" [With,To,About]
lexicon "talk" = talk Infl "talk" [With,To,About]
lexicon "loved" = love Tense "loved"
lexicon "love" = love Infl "love"
lexicon "shared" = love Tense "shared"
lexicon "share" = love Infl "share"
lexicon "told" = tell Tense "told"
lexicon "tell" = tell Infl "tell"


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
lexicon "through"   = prep "Through"
lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]
lexicon "as"   = conj "as"
lexicon "but"   = conj "but"

lexicon _ = []

testSuite1 :: [String]
testSuite1 = 
 [
     "Who did Sean tell?"
     ,"What did Sean tell?"
     ,"What did Sean tell Beverly?"
     ,"What thing did Sean tell Beverly?"
     ,"Who did Sean tell some thing?"
     ,"Who did Sean tell a lie?"
     -- ,"Who told Sean?"
     -- ,"Who told Sean some thing?"
     -- ,"Who told Sean that she loved him?"
     -- ,"Who did Sean tell that he loved her?"
     ,"To whom did Sean talk about the happiness that they shared?"
--   ,"Who talked about Sean?"
     -- ,"What did Sean talk about?"
--   ,"What did Beverly and Sean talk about?"
     ,"Who did Sean love?"
--   ,"Who did Beverly love?"
--   ,"Who did crawl through the phone?"

   ]

testSuite2 :: [String]
testSuite2 = 
 [
   "Sean told Beverly",
   "Sean told a lie.",
   "Sean told Beverly a lie.",
   "Did Sean tell Beverly a lie?",
   "Sean told Beverly a lie.",
   "Sean talked to Beverly.",
   "Sean talked about happiness.",
   "The woman who Sean loved loved Sean.",
   "The woman who Sean loved didn't love Sean",
   "Beverly shared happiness with Sean.",
   "The man who shared happiness with Sean was Beverly."
   ]

