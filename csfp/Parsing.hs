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
	  | Pos   | Ng
          | About | After | Around | At | As | BecauseOf
	  | In | Like | On | For | With
	  | By | To | From | Through
	  | Of
          deriving (Eq,Show,Ord)

type Agreement = [Feat]

gender, number, person, gcase, pronType, tense, prepType, advType, posType, polarity
		 :: Agreement -> Agreement
gender   = filter (`elem` [MascOrFem,Masc,Fem,Neutr])
number   = filter (`elem` [Sg,Pl])
person   = filter (`elem` [Fst,Snd,Thrd])
gcase    = filter (`elem` [Nom,AccOrDat])
pronType = filter (`elem` [Pers,Refl,Wh]) 
tense    = filter (`elem` [Tense,Infl]) 
prepType = filter (`elem` [About,After,As,At,BecauseOf,Like,In,On,For,With,By,To,From,Through]) 
advType = filter (`elem` [Around,In,On]) 
posType  = filter (`elem` [Of])
polarity  = filter (`elem` [Pos,Ng])

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
           length (prepType feats) <= 1,
           length (polarity feats) <= 1
	   ]
  where 
    feats = (prune . nub . sort) (fs cat1 ++ fs cat2)

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not (null (combine cat1 cat2))

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = 
  [Cat phon label fs' subcatlist | 
         fs' <- combine c (Cat "" "" [f] []) ]

toupper = reverse . upperizer . reverse where
        upperizer (x:[]) = toUpper x : []
	upperizer (x:'_':xs) = (toUpper x) : '_': (upperizer xs)
	upperizer (x:xs) = x : upperizer xs

type Lexset = [ [Cat] ]

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

preproc("what's":xs)          = "what" : "was" : preproc xs
preproc("what're":xs)          = "what" : "were" : preproc xs

preproc ("grow":"up":xs)	= "grow_up" : preproc xs
preproc ("grew":"up":xs)	= "grew_up" : preproc xs
preproc ("take":"care":"of":xs)	= "take_care_of" : preproc xs
preproc ("took":"care":"of":xs)	= "took_care_of" : preproc xs
preproc ("looked":"after":xs)	= "looked_after" : preproc xs
preproc ("look":"after":xs)	= "look_after" : preproc xs
preproc ("cared":"for":xs)	= "cared_for" : preproc xs
preproc ("care":"for":xs)	= "care_for" : preproc xs
preproc ("bowel":"movement":xs)	= "bowel_movement" : preproc xs
preproc ("bowel":"movements":xs)	= "bowel_movements" : preproc xs
preproc ("water":"sports":xs)	= "water_sports" : preproc xs
preproc ("surfing":"accident":xs)	= "surfing_accident" : preproc xs

preproc ("throat":"cancer":xs)	= "throat_cancer" : preproc xs
preproc ("shrinking":"violet":xs)	= "shrinking_violet" : preproc xs
-- preproc ("turn":"around":xs)	= "turn_around" : preproc xs
-- preproc ("turned":"around":xs)	= "turned_around" : preproc xs
preproc ("hands":"on":"hips":xs)	= "hands_on_hips" : preproc xs
preproc ("front":"desk":xs)	= "front_desk" : preproc xs
preproc ("movie":"star":xs)	= "movie_star" : preproc xs
preproc ("new":"york":xs)	= "new_york" : preproc xs
preproc ("south":"africa":xs)	= "south_africa" : preproc xs

preproc ("mr":"batchelor":xs)	= "mr_batchelor" : preproc xs
preproc ("mr":"payne":xs)	= "mr_payne" : preproc xs
preproc ("business":"law":xs)	= "business_law" : preproc xs
preproc ("rutgers":"university":xs)	= "rutgers_university" : preproc xs

preproc ("dr":"bean":xs)	= "dr_bean" : preproc xs
preproc ("european":"campers":xs)	= "european_campers" : preproc xs
preproc ("office":"worker":xs)	= "office_worker" : preproc xs
preproc ("production":"manager":xs)	= "production_manager" : preproc xs
preproc ("sales":"manager":xs)	= "sales_manager" : preproc xs
preproc ("slow":"living":xs)	= "slow_living" : preproc xs
preproc ("lack":"of":"control":xs)	= "lack_of_control" : preproc xs
preproc ("lack":"of":"support":xs)	= "lack_of_support" : preproc xs
preproc ("put":"pressure":xs)	= "put_pressure" : preproc xs
preproc ("feel":"stress":xs)	= "feel_stress" : preproc xs
preproc ("felt":"stress":xs)	= "felt_stress" : preproc xs

preproc ("eagle":"scout":xs)	= "eagle_scout" : preproc xs
preproc ("troop":"409":xs)	= "troop_409" : preproc xs
preproc ("assistant":"scoutmaster":xs)	= "assistant_scoutmaster" : preproc xs
preproc ("look":"back":xs)	= "look_back" : preproc xs
preproc ("looked":"back":xs)	= "looked_back" : preproc xs
preproc ("got":"married":xs)	= "got_married" : preproc xs
preproc ("get":"married":xs)	= "get_married" : preproc xs
preproc ("mentally":"disabled":xs)	= "mentally-disabled" : preproc xs
preproc ("physically":"disabled":xs)	= "physically-disabled" : preproc xs
preproc ("traffic":"accident":xs)	= "traffic_accident" : preproc xs
preproc ("brain":"damage":xs)	= "brain_damage" : preproc xs

preproc ("the":"state":"of":"colorado":xs) = "the_state_of_colorado" : preproc xs
preproc ("the":"gathering":"place":xs)  = "the_gathering_place" : preproc xs
preproc ("ten":"dollar":"bill":xs)      = "ten_dollar_bill" : preproc xs
preproc ("administrative":"assistant":xs) = "administrative_assistant" : preproc xs
preproc ("birthday":"card":xs)  = "birthday_card" : preproc xs

preproc ("because":"of":xs)	= "because_of" : preproc xs

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
	where unknownWord = [Cat "" "" [] []]

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

balancefs :: ParseTree Cat Cat -> [ Feat ]
balancefs t = 
	let feats = fs $ t2c t
		in map polarize feats
		where polarize feat = case feat of
			Pos -> Ng
			Ng  -> Pos
			otherwise -> feat


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

prsTAG :: SPARSER Cat Cat
prsTAG = \us xs -> [ (Branch (Cat "_" "S" [] []) [s,t],ws,zs) |
	(s,vs,ys) <- spR us xs,
	(t,ws,zs) <- tagR vs ys ]

tagR :: SPARSER Cat Cat 
tagR = \ us xs -> 
 [ (Branch (Cat "_" "TAG" (fs (t2c cop)) []) [tagV,tagS],ws,zs) | 
	(tagV,vs,ys)	<- leafPS "TAG" us xs,
	(cop,ws,zs)	<- pop "TAG" vs ys,
	agreeC tagV cop,
	(tagS,ps,qs)	<- leafPS "NP" ws zs,
	(subj,rs,ss)	<- pop "TAG" ps qs,
	agreeC tagS subj
	]

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
prsNP = leafPS "NP" <||> npR <||> npADJR <||> npposR <||> cnposR <||> adjcnposR <||> depCR  <||> pop "NP" 

npR :: SPARSER Cat Cat
npR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn], (us++ws'), zs) | 
      (det,vs,ys) <- prsDET [] xs,
      (cn,ws,zs)  <- prsCN vs ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn,
      tag         <- [Cat (phon (t2c cn)) "TAG" fs [] ],
      ws'         <- case us of [] -> [tag:ws]; otherwise -> [ws]
      ]

npADJR :: SPARSER Cat Cat
npADJR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,adj,cn], (us++ss), ts) | 
      (det,vs,ys) <- prsDET [] xs,
      (adj,ws,zs)  <- prsADJ vs ys,
      (cn,ss,ts)  <- prsCN ws zs,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]

npposR :: SPARSER Cat Cat
npposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [np,pos,cn], (us++ss), ts) |
      (np,vs,ys) <- leafPS "NP" [] xs,
      (pos,ws,zs) <- prsAPOS vs ys,
      (cn,ss,ts)  <- prsCN ws zs
      ]

cnposR :: SPARSER Cat Cat
cnposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [Branch (Cat "_" "NP" fs []) [det,cn1],pos,cn2], (us++qs), rs) |
      (det,vs,ys) <- prsDET [] xs,
      (cn1,ws,zs)  <- prsCN vs ys,
      fs         <- combine (t2c det) (t2c cn1),
      agreeC det cn1,
      (pos,ss,ts) <- prsAPOS ws zs,
      (cn2,qs,rs)  <- prsCN ss ts
      ]

adjcnposR :: SPARSER Cat Cat
adjcnposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [Branch (Cat "_" "NP" fs []) [det,adj,cn1],pos,cn2], (us++os), ps) |
      (det,vs,ys) <- prsDET [] xs,
      (adj,ws,zs)  <- prsADJ vs ys,
      (cn1,ss,ts)  <- prsCN ws zs,
      fs         <- combine (t2c det) (t2c cn1),
      agreeC det cn1,
      (pos,qs,rs) <- prsAPOS ss ts,
      (cn2,os,ps)  <- prsCN qs rs
      ]

depCR :: SPARSER Cat Cat
depCR = \us xs ->
  [ (Branch (Cat "_" "NP" (fs (t2c vp)) []) [ing,Branch (Cat "_" "VP" [] []) [vp,xps] ], (us++ss), ts) |
      (vp,vs,ys)  <- leafPS "VP" us xs, -- TODO us? []?
      (ing,ws,zs)  <- prsGER vs ys,
      verb	<- [t2c vp],
      (xps,ss,ts)  <- vpR ws (verb:zs)
      ]

prsGER :: SPARSER Cat Cat
prsGER = leafPS "GER"

prsAPOS :: SPARSER Cat Cat
prsAPOS = leafPS "APOS"

prsOFPOS :: SPARSER Cat Cat
prsOFPOS = leafPS "OFPOS"

prsZERO :: SPARSER Cat Cat
prsZERO = succeedS $ Leaf (Cat "zero" "DET" [Pl] [])

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET" <||> prsZERO 

prsADJ :: SPARSER Cat Cat
prsADJ = leafPS "ADJ"

prsCN :: SPARSER Cat Cat
prsCN = leafPS "CN" <||> cnrelR <||> ofR

prsVP :: SPARSER Cat Cat
prsVP = finVpR <||> auxVpR <||> copR

copR :: SPARSER Cat Cat
copR = \us xs -> [(Branch (Cat "_" "VP" (fs (t2c cop)) []) [cop,Branch (Cat "_" "COMP" [] []) [comp]],ws,zs) |
	(cop,vs,ys)  <- prsCOP us xs,
	tag         <- [Cat (phon (t2c cop)) "TAG" (balancefs cop) [] ],
	(comp,ws,zs)  <- push tag prsCOMP vs ys
		]

prsCOP :: SPARSER Cat Cat
prsCOP = leafPS "COP" <||> pop "COP"

prsCOMP :: SPARSER Cat Cat
prsCOMP = prsNP <||> prsADJ

vpR :: SPARSER Cat Cat
vpR = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),ws,zs) |  
             (vp,vs,ys)  <- leafPS "VP" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (xps,ws,zs) <- prsVdependents vs ys, 
             match subcatlist (map t2c xps) ]

finVpR :: SPARSER Cat Cat
finVpR = \us xs -> [(vp',us++vs,ys) | 
		tag        <- [Cat "did" "TAG" [ Ng ] [] ],
		(vp,vs,ys) <- push tag vpR [] xs,
		vp'        <- assignT Tense vp ]

auxVpR :: SPARSER Cat Cat
auxVpR = \us xs -> [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
	[aux,inf'], ws, zs) | 
	(aux,vs,ys) <- prsAUX us xs,
	tag        <- [Cat (phon (t2c aux)) "TAG" (balancefs aux) []],
	(inf,ws,zs) <- push tag vpR vs ys,
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

prsVdependents = prsNPsorPPs <||> manyS prsS

cnrelR :: SPARSER Cat Cat
cnrelR = \us xs -> 
     [ (Branch (Cat "_" "CN" (fs (t2c cn)) []) 
               [cn,rel], ws, zs) |
                 (cn,vs,ys)  <- leafPS "CN" us xs, 
                 (rel,ws,zs) <- prsREL vs ys, 
                 agreeC cn rel ]

ofR :: SPARSER Cat Cat
ofR = \us xs ->
  [ (Branch (Cat "_" "CN" (fs (t2c cn1)) []) [cn1,pos,np2], (us++ss), ts) |
      (cn1,vs,ys)  <- leafPS "CN" us xs,
      (pos,ws,zs) <- prsOFPOS vs ys,
      (np2,ss,ts) <- prsNP ws zs
      ]

prsREL :: SPARSER Cat Cat 
prsREL = relclauseR <||> thatlessR <||> relppR

relclauseR :: SPARSER Cat Cat
relclauseR = \us xs -> 
  [(Branch (Cat "_" "MOD" fs []) [rel,s], ws, zs) |
      (rel,vs,ys) <- leafPS "REL" us xs, 
       fs         <- [fs (t2c rel)],
       gap        <- [Cat "#" "NP" fs []],
       (s,ws,zs)  <- push gap prsS vs ys ]

thatlessR :: SPARSER Cat Cat 
thatlessR = \ us xs -> 
        [ (Branch (Cat "_" "MOD" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [AccOrDat] []], 
           (s,vs,ys) <- push gap prsS us xs, 
           notElem Wh (fs (t2c s))                       ]

relppR :: SPARSER Cat Cat
relppR = \us xs -> 
     [ (Branch (Cat "_" "MOD" [] []) [pp], vs, ys) |
                 (pp,vs,ys) <- prsPP us xs ] 

prsYN :: SPARSER Cat Cat 
prsYN = \us xs -> 
   [(Branch (Cat "_" "YN" [] []) [dum,s], ws,zs) | 
       (dum,vs,ys) <- prsDUM us xs, 
       gap         <- [Cat "#" (catLabel (t2c dum)) (fs (t2c dum)) [] ], 
       (s,ws,zs)   <- push gap prsS vs ys ]

prsDUM :: SPARSER Cat Cat
prsDUM = leafPS "COP" <||> prsAUX

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
