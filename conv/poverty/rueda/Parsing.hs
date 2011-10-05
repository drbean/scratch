module Parsing where

import Data.List
import Data.Char

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = "Leaf " ++ show t
  show (Branch l ts) = "\n[Branch " ++ "\t" ++ show l  ++ "\t" 
                            ++ show ts ++ "]"
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
          | At | As | In | On | For | With | By | To | From | Through
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
prepType = filter (`elem` [As,At,In,On,For,With,By,To,From,Through]) 

prune :: Agreement -> Agreement
prune fs = if   (Masc `elem` fs || Fem `elem` fs)
           then (delete MascOrFem fs) 
           else fs 

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = label ++ show agr
  show (Cat phon label agr subcats) = phon  ++ " " ++ label ++ show agr ++ show subcats

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

type Lexsets = [ (String, [Cat]) ]
people_names, object_names, class_names, prons, reflexives, interrogatives, aux, intransitives, transitives, ditransitives, determiners, preps, conjuncts :: Lexsets

people_names = [
	("pj",	[Cat "pj"	"NP" [Thrd,Fem,Sg] []]),
	("ann",	[Cat "ann"	"NP" [Thrd,Fem,Sg] []]),
	("dan",	[Cat "dan"	"NP" [Thrd,Masc,Sg] []]),
	("sam",	[Cat "sam"	"NP" [Thrd,Masc,Sg] []]),
	("dick",	[Cat "dick"	"NP" [Thrd,Masc,Sg] []])
	]

object_names = [
	("phone_number", [Cat "phone_number"	"CN" [Thrd,Neutr,Sg] []]),
	("fake_id", [Cat "fake_id" "CN" [Thrd,Neutr,Sg] []]),
	("story", [Cat "story" "CN" [Thrd,Neutr,Sg] []]),
	("bar", [Cat "bar" "CN" [Thrd,Neutr,Sg] []]),
	("teacher", [Cat "teacher" "CN" [Thrd,MascOrFem,Sg] []])
	]

class_names = [
--noun and verb
	("parent", [Cat "parent" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
		Cat "parent" "CN" [Sg,MascOrFem,Thrd]   []]),
	("parents",	[Cat "parents" "CN" [Pl,MascOrFem,Thrd]   []]),
	("mother",  [Cat "mother" "CN" [Sg,Fem,Thrd] []]),
	("daughter", [Cat "daughter" "CN" [Sg,Fem,Thrd] []]),
	("father",  [Cat "father"    "CN" [Sg,Masc,Thrd] []]),
	("son",     [Cat "son"    "CN" [Sg,Masc,Thrd] []]),
	("boyfriend",	[Cat "boyfriend"	"CN" [Thrd,Masc,Sg] []]),
	("girlfriend",	[Cat "girlfriend"	"CN" [Thrd,Fem,Sg] []]),
	("thing",   [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]),
	("things",  [Cat "things"  "CN" [Pl,Neutr,Thrd] []]),
	("person",  [Cat "person"  "CN" [Sg,Masc,Thrd]  []]),
	("persons", [Cat "persons" "CN" [Pl,Masc,Thrd]  []]),
	("boy",     [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]),
	("boys",    [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]),
	("man",     [Cat "man"     "CN" [Sg,Masc,Thrd]  []]),
	("men",     [Cat "men"     "CN" [Pl,Masc,Thrd]  []]),
	("girl",    [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]),
	("girls",   [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]),
	("woman",   [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]),
	("women",   [Cat "women"   "CN" [Pl,Fem,Thrd]   []])
	]

prons = [
	("i",   [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]),
	("me",  [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]),
	("we",  [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]),
	("us",  [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]),
	("you", [Cat "you" "NP" [Pers,Snd]               []]),
	("he",  [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]),
	("him", [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] []]),
	("she", [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]),
	("her", [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] []]),
	("it",  [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]),
	("they", [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]),
	("them", [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] []])
	]

reflexives = [
	("myself",     [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]),
	("ourselves",  [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]),
	("yourself",   [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]),
	("yourselves", [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]),
	("himself",    [Cat "himself"   "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]),
	("herself",    [Cat "herself"   "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]),
	("itself",     [Cat "itself"    "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]),
	("themselves", [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []])
	]

interrogatives = [
	("who",     [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [],
			Cat "who" "REL" [MascOrFem]         []]),
	("whom",    [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [],
			Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]),
	("what",    [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]),
	("that",    [Cat "that"  "REL" [] [], Cat "that"  "DET" [Sg]    []]),
	("which",   [Cat "which" "REL" [Neutr] [], Cat "which" "DET" [Wh] []])
	]

determiners = [
	("every",   [Cat "every"   "DET" [Sg]  []]),
	("all",     [Cat "all"     "DET" [Pl]  []]),
	("some",    [Cat "some"    "DET" []    []]),
	("several", [Cat "several" "DET" [Pl]  []]),
	("a",       [Cat "a"       "DET" [Sg]  []]),
	("no",      [Cat "no"      "DET" []    []]),
	("the",     [Cat "the"     "DET" []    []]),
	("most",    [Cat "most"    "DET" [Pl]  []]),
	("many",    [Cat "many"    "DET" [Pl]  []]),
	("few",     [Cat "few"     "DET" [Pl]  []]),
	("this",    [Cat "this"    "DET" [Sg]  []]),
	("these",   [Cat "these"   "DET" [Pl]  []]),
	("those",   [Cat "those"   "DET" [Pl]  []]),
	("less_than", [Cat "less_than" "DF" [Pl] []]),
	("more_than", [Cat "more_than" "DF" [Pl] []])
	]

aux = [
	("did",    [Cat "did"    "AUX" [] []]),
	("didn't", [Cat "didn't" "AUX" [] []])
	]

intransitives = [
	("worked",
	[Cat "worked"    "VP" [Tense] [],
			Cat "worked"	"VP" [Tense] [Cat "_" "PP" [As] []]] ),
	("work",     [Cat "work"     "VP" [Infl]  [],
			Cat "work"	"VP" [Infl] [Cat "_" "PP" [As] []]]),
	("yelled",    [Cat "yelled"    "VP" [Tense] [],
			Cat "yelled"	"VP" [Tense] [Cat "_" "PP" [At] []]]),
	("yell",     [Cat "yell"     "VP" [Infl]  [],
			Cat "yell"	"VP" [Infl] [Cat "_" "PP" [At] []]]),
	("separated",    [Cat "separated"    "VP" [Tense] []] ),
	("separate",     [Cat "separate"     "VP" [Infl]  []] )
	]

transitives = [
	("contacted", [Cat "contacted" "VP" [Tense]
						[Cat "_" "NP" [AccOrDat] []]]),
	("contact", [Cat "contact" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]),
	("had",	[Cat "had"	"VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]),
	("have", [Cat "have"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]),
	("married", [Cat "married" "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]),
	("marry", [Cat "marry"	"VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]),
	("divorced", [Cat "divorced" "VP" [Tense]
						[Cat "_" "NP" [AccOrDat] []]]),
	("divorce", [Cat "divorce" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]),
	("parented", [Cat "parented" "VP" [Tense]
						[Cat "_" "NP" [AccOrDat] []]])
	]

ditransitives = [
	("met",         [Cat "met" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In]       []]]),
	("meet",         [Cat "meet" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In]       []]]),
	("asked",	[Cat "asked" "VP" [Tense] [Cat "_" "PP" [For] []],
			Cat "asked" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
					Cat "_" "PP" [For]       []]]),
	("ask",	[Cat "ask" "VP" [Infl] [Cat "_" "PP" [For]       []],
		 Cat "ask" "VP" [Infl]	[Cat "_" "NP" [AccOrDat] [],
					 Cat "_" "PP" [For]       []]]),
	("put",         [Cat "put" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In]       []],
			Cat "put" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [In]       []]]),
	("handed",	[Cat "handed" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
							Cat "_" "PP" [To] []],
			Cat "handed" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat]  []]]),
	("hand",         [Cat "hand" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To] []],
			Cat "hand" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]),
	("gave",         [Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "gave" "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat]  []]]),
	("give",         [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]),
	("told",        [Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] []],
			Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "told" "VP" [Tense]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]]),
	("tell",        [Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []],
			Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "PP" [To]       []],
			Cat "tell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
						Cat "_" "NP" [AccOrDat] []]])
	]

preps = [
	("as",   [Cat "as"   "PREP" [As]   []]),
	("at",   [Cat "at"   "PREP" [At]   []]),
	("by",   [Cat "by"   "PREP" [By]   []]),
	("for",  [Cat "for"  "PREP" [For]  []]),
	("from", [Cat "from" "PREP" [From] []]),
	("in",   [Cat "in"   "PREP" [In]   []]),
	("on",   [Cat "on"   "PREP" [On]   []]),
	("to",   [Cat "to"   "PREP" [To]   []]),
	("through", [Cat "through" "PREP" [Through] []]),
	("with", [Cat "with" "PREP" [With] []])
	]

conjuncts = [
	("and",   [Cat "and"  "CONJ" [] []]),
	(".",     [Cat "."    "CONJ" [] []]),
	("if",    [Cat "if"   "COND" [] []]),
	("then",  [Cat "then" "THEN" [] []])
	]

lexicon :: String -> [Cat]

lexicon lexeme = maybe [Cat "_" "" [] []] id $ lookup lexeme $
	people_names ++ object_names ++ class_names ++
	prons ++ reflexives ++ interrogatives ++
	aux ++ intransitives ++ transitives ++ ditransitives ++
	preps ++ determiners ++ conjuncts

scan :: String -> String
scan []                      = []
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

preproc ("phone":"number":xs)	= "phone_number" : preproc xs
preproc ("fake":"id":xs)	= "fake_id" : preproc xs

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
    unknown = map fst (filter (null.snd) listing)
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

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET"

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
