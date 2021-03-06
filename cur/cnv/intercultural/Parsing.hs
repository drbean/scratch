module Parsing where

import Data.List
import Data.Char

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = "\n\tLeaf " ++ "(" ++ show t ++ ")"
  show (Branch l ts) = "\nBranch " ++ "\t" ++ show l  ++ "\t" 
                            ++ show ts ++ "\n"
type Pos = [Int]

pos ::  ParseTree a b -> [Pos]
pos Ep            = [[]]
pos (Leaf _)      = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, 
                                     p <- pos t ]

subtree :: ParseTree a b -> Pos -> ParseTree a b 
subtree t             []     = t
subtree (Branch _ ts) (i:is) = subtree (ts!!i) is 

subtrees :: ParseTree a b -> [ParseTree a b]
subtrees t = [ subtree t p | p <- pos t ]


data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat
          | Pers  | Refl | Wh 
          | Tense | Infl
          | About | At | As | In | On | For | With | By | To | From | Through
	  | Of
          deriving (Eq,Show,Ord)

type Agreement = [Feat]

gender, number, person, gcase, pronType, tense, prepType 
		 :: Agreement -> Agreement
gender   = filter (`elem` [MascOrFem,Masc,Fem,Neutr])
number   = filter (`elem` [Sg,Pl])
person   = filter (`elem` [Fst,Snd,Thrd])
gcase    = filter (`elem` [Nom,AccOrDat])
pronType = filter (`elem` [Pers,Refl,Wh]) 
tense    = filter (`elem` [Tense,Infl]) 
prepType = filter (`elem` [About,As,At,In,On,For,With,By,To,From,Through]) 
posType  = filter (`elem` [Of])

prune :: Agreement -> Agreement
prune fs = if   (Masc `elem` fs || Fem `elem` fs)
           then (delete MascOrFem fs) 
           else fs 

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = "Cat " ++ label ++ show agr ++ show subcatlist
  show (Cat phon label agr subcats) = "Cat " ++ phon  ++ " " ++ label ++ show agr ++ show subcats

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement 
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 = 
 [ feats | length (gender   feats) <= 1, 
           length (number   feats) <= 1, 
           length (person   feats) <= 1, 
           length (gcase    feats) <= 1,
           length (pronType feats) <= 1,
           length (tense    feats) <= 1,
           length (prepType feats) <= 1 ]
  where 
    feats = (prune . nub . sort) (fs cat1 ++ fs cat2)

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not (null (combine cat1 cat2))

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = 
  [Cat phon label fs' subcatlist | 
         fs' <- combine c (Cat "" "" [f] []) ]

type Lexset = [ [Cat] ]
proper_names, object_names, class_names, prons, reflexives, interrogatives, aux, intransitives, transitives, ditransitives, determiners, preps, conjuncts :: Lexset

collect_lex = [
	("auxiliary verbs", aux),
	("intransitive verbs", intransitives),
	("transitive verbs", transitives),
	("ditransitive verbs", ditransitives),
	("object_names", object_names),
	("class_names", class_names),
	("prepositions", preps),
	("determiners", determiners),
	("possessives", possessives)
	]

toupper = reverse . upperizer . reverse where
        upperizer (x:[]) = toUpper x : []
	upperizer (x:'_':xs) = (toUpper x) : '_': (upperizer xs)
	upperizer (x:xs) = x : upperizer xs

proper_names = [
	[Cat "english" "NP" [Thrd,Neutr,Sg] []],
	[Cat "cuba" "NP" [Thrd,Neutr,Sg] []],
	[Cat "spanish" "NP" [Thrd,Neutr,Sg] []],
	[Cat "the_united_states" "NP" [Thrd,Neutr,Sg] []],
	[Cat "gus"	"NP" [Thrd,Masc,Sg] []],
	[Cat "ileana"	"NP" [Thrd,Fem,Sg] []],
	[Cat "ofelia"	"NP" [Thrd,Fem,Sg] []],
	[Cat "fidel"	"NP" [Thrd,Masc,Sg] []]
	]

object_names = [
	[Cat "medical_school" "NP" [Thrd,Neutr,Sg] []],
	[Cat "medicine" "NP" [Thrd,Neutr,Sg] []],
	[Cat "doctor" "CN" [Thrd,Neutr,Sg] []],
	[Cat "doll" "CN" [Thrd,Neutr,Sg] []],
	[Cat "dolls" "CN" [Thrd,Neutr,Pl] []],
	[Cat "present" "CN" [Thrd,Neutr,Sg] []],
	[Cat "tomato" "CN" [Thrd,Neutr,Sg] []],
	[Cat "tomatoes" "CN" [Thrd,Neutr,Pl] []],
	[Cat "fields" "CN" [Thrd,Neutr,Pl] []],
	[Cat "cleaning" "NP" [Thrd,Neutr,Sg] []],
	[Cat "motel" "CN" [Thrd,Neutr,Sg] []],
	[Cat "boat" "CN" [Thrd,Neutr,Sg] []],
	[Cat "money" "NP" [Thrd,Neutr,Sg] []],
	[Cat "disappointment" "CN" [Thrd,Neutr,Sg] []],
	[Cat "upbringing" "CN" [Thrd,Neutr,Sg] []],
	[Cat "story" "CN" [Thrd,Neutr,Sg] []]
	]

class_names = [
--noun and verb
	[Cat "parent" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "parent" "CN" [Sg,MascOrFem,Thrd]   []],
	[Cat "parents" "CN" [Pl,MascOrFem,Thrd]   []],
	[Cat "father"    "CN" [Sg,Masc,Thrd] []],
	[Cat "mother"    "CN" [Sg,Fem,Thrd] []],
	[Cat "daughter"    "CN" [Sg,Fem,Thrd] []],
	[Cat "son"    "CN" [Sg,Masc,Thrd] []],
	[Cat "brother"    "CN" [Sg,Masc,Thrd] []],
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
	[Cat "women"   "CN" [Pl,Fem,Thrd]   []]
	]

possessives = [
	[Cat "'s" "APOS" [] []]
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

intransitives = [
	[Cat "got_married"    "VP" [Tense] [],
		Cat "got_married"	"VP" [Tense] [Cat "_" "PP" [In] []]],
	[Cat "get_married"     "VP" [Infl]  [],
		Cat "get_married"	"VP" [Infl] [Cat "_" "PP" [In] []]]
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

ditransitives = [
	[Cat "studied" "VP" [Tense] [Cat "_" "PP" [At] []],
		Cat "studied" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []],
		Cat "studied" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [At] []]],
	[Cat "study" "VP" [Infl] [Cat "_" "PP" [At] []],
		Cat "study" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "study" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [At] []]],
	[Cat "worked" "VP" [Tense] [],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [As,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [At,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [On,Neutr] []],
		Cat "worked" "VP" [Tense] [Cat "_" "PP" [In,Neutr] []]],
	[Cat "work" "NP" [AccOrDat] [],
		Cat "work" "CN" [AccOrDat] [],
		Cat "work" "VP" [Infl] [],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [As,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [At,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [On,Neutr] []],
		Cat "work" "VP" [Infl] [Cat "_" "PP" [In,Neutr] []]],
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
	-- [Cat "in"   "PREP" [In]   []],
	[Cat "on"   "PREP" [On]   []],
	[Cat "to"   "PREP" [To]   []],
	[Cat "through" "PREP" [Through] []],
	[Cat "with" "PREP" [With] []]
	]

conjuncts = [
	[Cat "and"  "CONJ" [] []],
	[Cat "but"  "CONJ" [] []],
	[Cat "."    "CONJ" [] []]
	-- [Cat "if"   "COND" [] []],
	-- [Cat "then" "THEN" [] []]
	]

--lexicon :: String -> [Cat]

unknownWord = [Cat "" "" [] []]

lexicon lexeme = maybe unknownWord id $
	find (\x -> phon (head x) == lexeme ) $
	proper_names ++ object_names ++ class_names ++
	prons ++ reflexives ++ interrogatives ++
	aux ++ intransitives ++ transitives ++ ditransitives ++
	possessives ++ preps ++ determiners ++ conjuncts

scan :: String -> String
scan []                      = []
scan ('\'':'s':xs)           = " 's" ++ scan xs
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

type Words = [String]

lexer :: String -> Words 
lexer = preproc . words . (map toLower) . scan



preproc :: Words -> Words
preproc []                 = []
preproc ["."]              = []
preproc ["?"]              = []
preproc (",":xs)           = preproc xs

preproc ("medical":"school":xs)	= "medical_school" : preproc xs
preproc ("the":"united":"states":xs)	= "the_united_states" : preproc xs
preproc ("look":"back":xs)	= "look_back" : preproc xs
preproc ("looked":"back":xs)	= "looked_back" : preproc xs
preproc ("got":"married":xs)	= "got_married" : preproc xs
preproc ("get":"married":xs)	= "get_married" : preproc xs

preproc ("an":xs)	= "a" : preproc xs
preproc ("did":"not":xs)   = "didn't" : preproc xs
preproc ("nothing":xs)     = "no"    : "thing"  : preproc xs
preproc ("nobody":xs)      = "no"    : "person" : preproc xs
preproc ("no-one":xs)      = "no"    : "person" : preproc xs
preproc ("no":"one":xs)    = "no"    : "person" : preproc xs
preproc ("something":xs)   = "some"  : "thing"  : preproc xs
preproc ("somebody":xs)    = "some"  : "person" : preproc xs
preproc ("someone":xs)    = "some"  : "person" : preproc xs
preproc ("everything":xs)  = "every" : "thing"  : preproc xs
preproc ("everybody":xs)   = "every" : "person" : preproc xs
preproc ("everyone":xs)   = "every" : "person" : preproc xs
preproc ("less":"than":xs) = "less_than" : preproc xs
preproc ("more":"than":xs) = "more_than" : preproc xs
preproc ("at":"least":xs)  = "at_least"  : preproc xs
preproc ("at":"most":xs)   = "at_most"   : preproc xs
preproc (x:xs)             = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = db w

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words = 
  let
    listing = map (\ x -> (x,lookupWord db x)) words
    unknown = map fst (filter (\x -> snd x == unknownWord) listing)
  in
    if unknown /= [] then 
      error ("unknown words: " ++ show unknown)
    else initCats (map snd listing) 

initCats :: [[Cat]] -> [[Cat]]
initCats []         = [[]]
initCats (cs:rests) = [ c:rest | c    <- cs, 
                                 rest <- initCats rests ]

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

agreeC :: ParseTree Cat Cat -> ParseTree Cat Cat -> Bool
agreeC t1 t2 = agree (t2c t1) (t2c t2) 

assignT :: Feat ->  ParseTree Cat Cat 
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]


match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y 
	      && agree x y 
	      && match xs ys 


type StackParser a b = [a] -> [a] -> [(b,[a],[a])]

type SPARSER a b = StackParser a (ParseTree a b)

infixr 4 <||>

(<||>) :: StackParser a b -> StackParser a b 
		  -> StackParser a b 
(p1 <||> p2) stack xs = p1 stack xs ++ p2 stack xs 

infixl 6 <::>

(<::>) :: StackParser a b  -> StackParser a [b] 
                           -> StackParser a [b]
(p <::> q) us xs = [(r:rs,ws,zs) | (r,vs,ys)  <- p us xs,
                                   (rs,ws,zs) <- q vs ys ]

succeedS :: b -> StackParser a b 
succeedS r us xs = [(r,us,xs)]

manyS :: StackParser a b -> StackParser a [b]
manyS p = (p <::> manyS p) <||> succeedS []

push :: Cat -> SPARSER Cat Cat -> SPARSER Cat Cat 
push c p stack = p (c:stack) 

pop :: CatLabel -> SPARSER Cat Cat 
pop c []     xs                   = []
pop c (u:us) xs | catLabel u == c = [(Leaf u, us, xs)]
                | otherwise       = []

leafPS :: CatLabel -> SPARSER Cat Cat
leafPS l _ []         = [] 
leafPS l s (c:cs) = [(Leaf c,s,cs) | catLabel c == l ]

prsTXT :: SPARSER Cat Cat
prsTXT = conjR <||> prsS

conjR :: SPARSER Cat Cat 
conjR = \ us xs -> 
   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], ws, zs) | 
       (s,vs,ys)      <- prsS us xs,
       (conj,vs1,ys1) <- leafPS "CONJ" vs ys, 
       (txt,ws,zs)    <- prsTXT vs1 ys1            ]

prsS :: SPARSER Cat Cat
prsS = spR <||> cond1R <||> cond2R

spR :: SPARSER Cat Cat 
spR = \ us xs -> 
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np',vp],ws,zs) | 
       (np,vs,ys) <- prsNP us xs,
       (vp,ws,zs) <- prsVP vs ys, 
        np'       <- assignT Nom np, 
       agreeC np vp,
       subcatList (t2c vp) == [] ]

cond1R :: SPARSER Cat Cat 
cond1R = \ us xs -> 
   [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
       (cond,vs,ys) <- leafPS "COND" us xs, 
       (s1,vs1,ys1) <- prsS vs ys,
       (s2,ws,zs)   <- prsS vs1 ys1 ]

cond2R :: SPARSER Cat Cat 
cond2R = \ us xs -> 
     [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
         (cond,vs,ys) <- leafPS "COND" us xs, 
         (s1,vs1,ys1) <- prsS vs ys,
         (_,vs2,ys2)  <- leafPS "THEN" vs1 ys1, 
         (s2,ws,zs)   <- prsS vs2 ys2 ]

prsNP :: SPARSER Cat Cat 
prsNP = leafPS "NP" <||> npR <||> pop "NP" 

npR :: SPARSER Cat Cat
npR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn], (us++ws), zs) | 
      (det,vs,ys) <- prsDET [] xs,
      (cn,ws,zs)  <- prsCN vs ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]

aposR :: SPARSER Cat Cat
aposR = \us xs ->
  [ (Branch (Cat "_" "DET" [] []) [pos,np1], (us++ws), zs) |
      (np1,vs,ys) <- leafPS "NP" us xs,
      (pos,ws,zs) <- prsAPOS vs ys
      ]

ofR :: SPARSER Cat Cat
ofR = \us xs ->
  [ (Branch (Cat "_" "NP" (fs (t2c np1)) []) [pos,np2,np1], (us++os), ps) |
      (np1,vs,ys) <- npR us xs,
      (pos,ws,zs) <- prsOFPOS vs ys,
      (np2,os,ps) <- leafPS "NP" ws zs
      ]

prsAPOS :: SPARSER Cat Cat
prsAPOS = leafPS "APOS"

prsOFPOS :: SPARSER Cat Cat
prsOFPOS = leafPS "OFPOS"

prsZERO :: SPARSER Cat Cat
prsZERO = succeedS $ Leaf (Cat "zero" "DET" [Pl] [])

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET" <||> aposR <||> prsZERO 

prsCN :: SPARSER Cat Cat
prsCN = leafPS "CN" <||> cnrelR

prsVP :: SPARSER Cat Cat
prsVP = finVpR <||> auxVpR

vpR :: SPARSER Cat Cat
vpR = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),ws,zs) |  
             (vp,vs,ys)  <- leafPS "VP" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (xps,ws,zs) <- prsNPsorPPs vs ys, 
             match subcatlist (map t2c xps) ]

finVpR :: SPARSER Cat Cat
finVpR = \us xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpR us xs,
                                   vp' <- assignT Tense vp ]

auxVpR :: SPARSER Cat Cat
auxVpR = \us xs -> 
     [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
               [aux,inf'], ws, zs) | 
                 (aux,vs,ys) <- prsAUX us xs,
                 (inf,ws,zs) <- vpR vs ys,
                  inf'       <- assignT Infl inf ] 

prsAUX :: SPARSER Cat Cat
prsAUX = leafPS "AUX" <||> pop "AUX" 

prsPP :: SPARSER Cat Cat
prsPP = ppR <||> pop "PP" 

ppR :: SPARSER Cat Cat
ppR = \us xs -> 
  [ (Branch (Cat "_" "PP" fs []) [prep,np'], ws, zs) | 
      (prep,vs,ys) <- prsPREP us xs, 
      (np,ws,zs)   <- prsNP vs ys,
       np'         <- assignT AccOrDat np, 
       fs          <- combine (t2c prep) (t2c np') ]

prsPREP :: SPARSER Cat Cat
prsPREP = leafPS "PREP"

prsNPorPP :: SPARSER Cat Cat
prsNPorPP = prsNP <||> prsPP

prsNPsorPPs :: [Cat] -> [Cat] 
       -> [([ParseTree Cat Cat],[Cat],[Cat])]
prsNPsorPPs = manyS prsNPorPP

cnrelR :: SPARSER Cat Cat
cnrelR = \us xs -> 
     [ (Branch (Cat "_" "CN" (fs (t2c cn)) []) 
               [cn,rel], ws, zs) |
                 (cn,vs,ys)  <- leafPS "CN" us xs, 
                 (rel,ws,zs) <- prsREL vs ys, 
                 agreeC cn rel ]

prsREL :: SPARSER Cat Cat 
prsREL = relclauseR <||> thatlessR 

relclauseR :: SPARSER Cat Cat
relclauseR = \us xs -> 
  [(Branch (Cat "_" "COMP" fs []) [rel,s], ws, zs) |
      (rel,vs,ys) <- leafPS "REL" us xs, 
       fs         <- [fs (t2c rel)],
       gap        <- [Cat "#" "NP" fs []],
       (s,ws,zs)  <- push gap prsS vs ys ]

thatlessR :: SPARSER Cat Cat 
thatlessR = \ us xs -> 
        [ (Branch (Cat "_" "COMP" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [AccOrDat] []], 
           (s,vs,ys) <- push gap prsS us xs, 
           notElem Wh (fs (t2c s))                       ]

prsYN :: SPARSER Cat Cat 
prsYN = \us xs -> 
   [(Branch (Cat "_" "YN" [] []) [aux,s], ws,zs) | 
       (aux,vs,ys) <- prsAUX us xs, 
       gap         <- [Cat "#" "AUX" (fs (t2c aux)) [] ], 
       (s,ws,zs)   <- push gap prsS vs ys ]

isWH :: ParseTree Cat Cat -> Bool
isWH tr = Wh `elem` (fs (t2c tr))

prsWH :: SPARSER Cat Cat 
prsWH = \us xs -> 
   [ (Branch (Cat "_" "WH" [] []) [wh,yn], ws,zs) | 
       (wh,vs,ys) <- prsNPorPP us xs, 
       isWH wh, 
       gapfs      <- [filter (/= Wh) (fs (t2c wh))],
       gap        <- [Cat "#" (catLabel (t2c wh)) gapfs []], 
       (yn,ws,zs) <- push gap prsYNS vs ys ]

prsYNS :: SPARSER Cat Cat
prsYNS = prsYN <||> spR

parses :: String -> [ParseTree Cat Cat]
parses str = let ws = lexer str 
             in  [ s | catlist   <- collectCats lexicon ws, 
                       (s,[],[]) <- prsWH [] catlist  
                                 ++ prsYN  [] catlist   
                                 ++ prsTXT  [] catlist ]

testSuite1 :: [String]
testSuite1 = 
 [ 
   "Who left Rebia?",
   "Frank gave Rebia the ring.",
   "Did Frank give the ring to Rebia?",
   "Who did Frank give the rings to?",
   "To whom did Frank give the rings?",
   "Frank left the woman " ++ "that he gave the rings to.",
   "Who killed the man that helped the woman " 
    ++ "that had a boyfriend." ]

testSuite2 :: [String]
testSuite2 =  
 [ "Jack loved the woman that Frank helped Jill",
   "Rebia loved the man that helped"
    ]
