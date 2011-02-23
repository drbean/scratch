module PP where

import Data.List
import Data.Char
import FPH

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = show t
  show (Branch l ts) = "\n[." ++ show l  ++ " " 
                            ++ show ts

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b 
succeed r xs = [(r,xs)]

infixr 4 <|>
 
(<|>) :: Parser a b -> Parser a b -> Parser a b 
(p1 <|> p2) xs = p1 xs ++ p2 xs 

(<*>) :: Parser a [b] -> Parser a [b] -> Parser a [b]
(p <*> q) xs = [ (r1 ++ r2,zs) | (r1,ys) <- p xs, 
                                 (r2,zs) <- q ys ]

infixl 7 <$>

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs = [ (f x,ys) | (x,ys) <- p xs ]

type PARSER a b = Parser a (ParseTree a b)

infixl 6 <:>

(<:>) :: Parser a b -> Parser a [b] -> Parser a [b]
(p <:> q) xs = [ (r:rs,zs) | (r,ys)  <- p xs, 
                             (rs,zs) <- q ys ]

many :: Parser a b -> Parser a [b]
many p = (p <:> many p) <|> (succeed [])

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat
          | Pers  | Refl | Wh 
          | Tense | Infl
          | About | Into | In   | On  | At  | With | By | To | From  
	  | Through
	  | During
          deriving (Eq,Show,Ord,Read)

string2feat :: String -> Feat
string2feat (first:others) = read prep where prep = (toUpper first) : others

type Agreement = [Feat]

gender, number, person, gcase, pronType, tense, prepType 
		 :: Agreement -> Agreement
gender   = filter (`elem` [MascOrFem,Masc,Fem,Neutr])
number   = filter (`elem` [Sg,Pl])
person   = filter (`elem` [Fst,Snd,Thrd])
gcase    = filter (`elem` [Nom,AccOrDat])
pronType = filter (`elem` [Pers,Refl,Wh]) 
tense    = filter (`elem` [Tense,Infl]) 
prepType = filter (`elem` [About,Into,In,At,On,With,By,To,From,Through
				,During
				]) 

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
  show (Cat phon label agr subcatlist) = phon  ++ " " ++ label ++ show agr ++
						  show subcatlist

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

-- lexical entry creators
woman :: String -> [Cat]
woman a = [Cat a "NP" [Thrd,Fem,Sg] []]
man :: String -> [Cat]
man a = [Cat a "NP" [Thrd,Masc,Sg] []]
thing a = [Cat a "CN" [Sg,Neutr,Thrd] []]
prep a = [Cat a "PREP" [ string2feat a ]   []]
conj a = [Cat a "CONJ" [] []]
-- verb lexical entry creators named after prototypical example
smile, love :: Feat -> String -> [Cat]
smile feat a    = [Cat a    "VP" [feat] []]
talk :: Feat -> String -> [Feat] -> [Cat]
talk feat a preps =
	[Cat a "VP" [feat] [], Cat a "VP" [feat] [Cat "_" "PP" preps []]]
love feat a = [ Cat a "VP" [feat] [Cat "_" "NP" [AccOrDat] []] ]
tell feat a = [Cat a "VP" [feat] [Cat "_" "NP" [AccOrDat] []],
	       Cat a "VP" [feat] [Cat "_" "NP" [AccOrDat] [],
				  Cat "_" "NP" [AccOrDat] []] ]
met :: String -> [Cat]
met a =
 [Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [At]     []], 
  Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
meet a =
 [Cat a   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
	                     Cat "_" "PP" [At]     []], 
  Cat a   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 
kicked :: String -> [Cat]
kicked a =
 [Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []], 
  Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
kick a =
 [Cat a "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []], 
  Cat a "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 
took :: String -> [Cat]
took a =
 [Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []], 
  Cat a "VP" [Tense] [Cat "_" "NP" [AccOrDat] []]]
take a =
 [Cat a "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []], 
  Cat a "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

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

preproc ("warm":"brown":"eyes":xs)   = "warm_brown_eyes" : preproc xs
preproc ("dark":"curly":"hair":xs)   = "dark_curly_hair" : preproc xs
preproc ("high":"school":"dance":xs)   = "high_school_dance" : preproc xs

preproc ("did":"not":xs)   = "didn't" : preproc xs
preproc ("nothing":xs)     = "no"    : "thing"  : preproc xs
preproc ("nobody":xs)      = "no"    : "person" : preproc xs
preproc ("something":xs)   = "some"  : "thing"  : preproc xs
preproc ("somebody":xs)    = "some"  : "person" : preproc xs
preproc ("everything":xs)  = "every" : "thing"  : preproc xs
preproc ("everybody":xs)   = "every" : "person" : preproc xs
preproc ("less":"than":xs) = "less_than" : preproc xs
preproc ("more":"than":xs) = "more_than" : preproc xs
preproc ("at":"least":xs)  = "at_least"  : preproc xs
preproc ("at":"most":xs)   = "at_most"   : preproc xs
preproc (x:xs)             = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = [ c | c <- db w ]

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

leafP :: CatLabel -> PARSER Cat Cat
leafP label []     = []
leafP label (c:cs) = [(Leaf c,cs) | catLabel c == label ]

assignT :: Feat ->  ParseTree Cat Cat 
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]

sRule :: PARSER Cat Cat
sRule = \ xs -> 
       [ (Branch (Cat "_" "S" [] []) [np',vp],zs) | 
         (np,ys) <- parseNP xs,
         (vp,zs) <- parseVP ys, 
         np'     <- assignT Nom np,
         agreeC np vp,
         subcatList (t2c vp) == [] ]

parseSent :: PARSER Cat Cat
parseSent = sRule 

npRule :: PARSER Cat Cat 
npRule = \ xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn],zs) | 
    (det,ys) <- parseDET xs, 
    (cn,zs)  <- parseCN  ys,
    fs       <- combine (t2c det) (t2c cn),
    agreeC det cn ]

parseNP :: PARSER Cat Cat
parseNP = leafP "NP" <|> npRule

ppRule :: PARSER Cat Cat
ppRule = \ xs -> 
   [ (Branch (Cat "_" "PP" fs []) [prep,np'],zs) | 
     (prep,ys) <- parsePrep xs, 
     (np,zs)   <- parseNP ys,
      np'      <- assignT AccOrDat np, 
      fs       <- combine (t2c prep) (t2c np') ]

parsePP :: PARSER Cat Cat
parsePP = ppRule 

parseNPorPP :: PARSER Cat Cat
parseNPorPP = parseNP <|> parsePP

parseNPsorPPs :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNPsorPPs = many parseNPorPP

detRule :: PARSER Cat Cat 
detRule = \ xs -> 
  [ (Branch (Cat "_" "DET" fs []) [df,num],zs) | 
    (df,ys) <- parseDF xs, 
--          (num,zs)  <- parseNUM  ys,
    (num,zs)  <- parseDIG  ys,
    fs       <- combine (t2c df) (t2c num),
    agreeC df num ]

parseDET :: PARSER Cat Cat
parseDET = leafP "DET" <|> detRule

parseDF :: PARSER Cat Cat
parseDF = leafP "DF"

parseDIG :: PARSER Cat Cat
parseDIG = leafP "DIG"

parseNUM :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNUM = many parseDIG

parseCN :: PARSER Cat Cat
parseCN = leafP "CN"

parsePrep :: PARSER Cat Cat
parsePrep = leafP "PREP"

parseAux :: PARSER Cat Cat
parseAux = leafP "AUX"

parseVP :: PARSER Cat Cat 
parseVP = finVpRule <|> auxVpRule

vpRule :: PARSER Cat Cat
vpRule = \xs -> 
 [ (Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),zs) |  
   (vp,ys)     <- leafP "VP" xs, 
   subcatlist  <- [subcatList (t2c vp)],
   (xps,zs)    <- parseNPsorPPs ys, 
   match subcatlist (map t2c xps) ]

match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y 
	      && agree x y 
	      && match xs ys 

finVpRule :: PARSER Cat Cat
finVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, 
			vp'    <- assignT Tense vp ]

auxVpRule :: PARSER Cat Cat
auxVpRule = \xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c aux)) []) [aux,inf'],zs) | 
  (aux,ys) <- parseAux xs,
  (inf,zs) <- vpRule ys, 
  inf'    <- assignT Infl inf ]   

prs :: ( String -> [Cat] ) -> String -> [ParseTree Cat Cat]
prs lexicon string = let ws = lexer string 
     in  [ s | catlist <- collectCats lexicon ws, 
	       (s,[])  <- parseSent catlist ]

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

--conjR :: SPARSER Cat Cat 
--conjR = \ us xs -> 
--   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], ws, zs) | 
--       (s, vs, ys)    <- prsTXT us xs,
--       (conj,vs1,ys1) <- leafPS "CONJ" vs ys, 
--       (txt, ws, zs)      <- prsS vs1 ys1 ]

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

detR :: SPARSER Cat Cat
detR = \ us xs -> 
  [ (Branch (Cat "_" "DET" fs []) [df,dig], (us++ws), zs) | 
      (df,vs,ys) <- prsDF [] xs, 
      (dig,ws,zs)  <- prsDIG vs ys,
       fs         <- combine (t2c df) (t2c dig),
      agreeC df dig ]

prsDF :: SPARSER Cat Cat
prsDF = leafPS "DF"

prsDIG :: SPARSER Cat Cat
prsDIG = leafPS "DIG"

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET" <||> detR

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
       (yn,ws,zs) <- push gap prsYN vs ys ]

parses :: ( String -> [Cat] ) -> String -> [ParseTree Cat Cat]
parses lexicon str = let ws = lexer str 
             in  [ s | catlist   <- collectCats lexicon ws, 
                       (s,[],[]) <- prsTXT [] catlist  
                                 ++ prsYN  [] catlist   
                                 ++ prsWH  [] catlist ]

data Term = Const String | Var Int deriving (Eq,Ord)

data GQ = Sm | All | Th | Most | Many | Few 
        deriving (Eq,Show,Ord) 

data Abstract = MkAbstract Int LF deriving (Eq,Ord) 

data LF = Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF] 
        | Qt GQ Abstract Abstract 
     deriving (Eq,Ord)

instance Show Term where
  show (Const name) = name 
  show (Var i)      = 'x': show i

instance Show Abstract where 
  show (MkAbstract i lf) = 
   "(\\ x" ++ show i ++ " " ++ show lf ++ ")"
  showList ls   s = showList__ shows ls s
    where
      showList__ _     []     s = "[]" ++ s
      showList__ showx (x:xs) s = '[' : showx x (showl xs)
        where
          showl []     = ']' : s
          showl (y:ys) = '\n' : showx y (showl ys)


instance Show LF where
  show (Rel r args)   = r ++ show args
  show (Eq t1 t2)     = show t1 ++ "==" ++ show t2
  show (Neg lf)       = '~': (show lf)
  show (Impl lf1 lf2) = "(" ++ show lf1 ++ "==>" 
                            ++ show lf2 ++ ")"
  show (Equi lf1 lf2) = "(" ++ show lf1 ++ "<=>" 
                            ++ show lf2 ++ ")"
  show (Conj [])      = "true" 
  show (Conj lfs)     = "conj" ++ concat [ show lfs ]
  show (Disj [])      = "false" 
  show (Disj lfs)     = "disj" ++ concat [ show lfs ]
  show (Qt gq a1 a2)   = show gq ++ (' ' : show a1) 
                                 ++ (' ' : show a2)

transS :: ParseTree Cat Cat -> LF
transS (Branch (Cat _ "S" _ _) [np,vp]) = 
  (transNP np) (transVP vp)

transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "did"    "AUX" _ []),s]) = transS s 
transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "didn't" "AUX" _ []),s]) = Neg (transS s)

transNP :: ParseTree Cat Cat -> 
                (Term -> LF) -> LF
transNP (Leaf (Cat "#"  "NP" _ _)) = \ p -> p (Var 0)
transNP (Leaf (Cat name "NP" _ _)) = \ p -> p (Const name)
transNP (Branch (Cat _ "NP" _ _) [det,cn]) = 
                             (transDET det) (transCN cn) 

transDET :: ParseTree Cat Cat -> (Term -> LF)
                              -> (Term -> LF) 
                              -> LF
transDET (Leaf (Cat "every" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt All       (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))

transDET (Leaf (Cat "all" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt All       (MkAbstract i (p (Var i)))  
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "some" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "a" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "several" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Sm      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "no" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Neg (Qt Sm (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i))))
transDET (Leaf (Cat "the" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Th        (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "most" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Most      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "many" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Qt Many      (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "few" "DET" _ _)) = 
  \ p q -> let i = fresh[p,q] in 
  Neg (Qt Many (MkAbstract i (p (Var i))) 
               (MkAbstract i (q (Var i))))

transDET (Leaf (Cat "which" "DET" _ _)) = 
  \ p q -> Conj [p (Var 0),q (Var 0)]

transCN :: ParseTree Cat Cat -> Term -> LF
transCN (Leaf   (Cat name "CN" _ _))          = \ x -> 
                                              Rel name [x]
transCN (Branch (Cat _    "CN" _ _) [cn,rel]) = \ x -> 
                       Conj [transCN cn x, transREL rel x]

transREL :: ParseTree Cat Cat -> Term -> LF
transREL (Branch (Cat _ "COMP" _ _ ) [rel,s]) = 
  \ x -> sub x (transS s)
transREL (Branch (Cat _ "COMP" _ _ ) [s])     = 
  \ x -> sub x (transS s)

transPP :: ParseTree Cat Cat -> (Term -> LF) -> LF
transPP (Leaf   (Cat "#" "PP" _ _)) = \ p -> p (Var 0)
transPP (Branch (Cat _   "PP" _ _) [prep,np]) = transNP np

transVP :: ParseTree Cat Cat -> Term -> LF
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [])]) = 
        \ t -> Rel name [t]
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_]),np]) = 
        \ subj -> transNP np (\ obj -> Rel name [subj,obj])

transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_,_]),np1,np2]) = 
        \ subj   -> transNP np1
        (\ iobj   -> transNP np2
         (\ obj -> Rel name [subj,obj,iobj]))
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "did" "AUX" _ []),vp]) = 
        transVP vp 
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "didn't" "AUX" _ []),vp]) = 
        \x -> Neg ((transVP vp) x)
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat "#" "AUX" _ []),vp]) = 
        transVP vp 

transWH :: ParseTree Cat Cat -> Abstract
transWH (Branch (Cat _ "WH" _ _ ) [wh,s]) = 
  MkAbstract 0 (Conj [transW wh, transS s])

transW :: ParseTree Cat Cat -> LF
transW (Branch (Cat _ "NP" fs _) [det,cn]) = 
                            transCN cn (Var 0)
transW (Leaf (Cat _ "NP" fs _)) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | MascOrFem `elem` fs = Rel "person" [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

transW (Branch (Cat _ "PP" fs _) [prep,np]) 
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | MascOrFem `elem` fs = Rel "person" [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

subst :: Term -> Term -> Term 
subst x (Const name)         = Const name
subst x (Var n) | n == 0     = x
                | otherwise  = Var n
                | x == Var n = error "bad substitution"

sub :: Term -> LF -> LF 
sub x (Rel name ts)     = Rel name (map (subst x) ts)
sub x (Eq t1 t2)        = Eq (subst x t1) (subst x t2)
sub x (Neg lf)          = Neg (sub x lf)
sub x (Impl lf1 lf2)    = Impl (sub x lf1) (sub x lf2)
sub x (Equi lf1 lf2)    = Equi (sub x lf1) (sub x lf2)
sub x (Conj lfs)        = Conj (map (sub x) lfs) 
sub x (Disj lfs)        = Disj (map (sub x) lfs) 
sub x (Qt gq abs1 abs2) = Qt gq (sb x abs1) (sb x abs2)

sb :: Term -> Abstract -> Abstract
sb x (MkAbstract 0 lf) = MkAbstract 0 lf
sb x (MkAbstract n lf) = MkAbstract n (sub x lf)

bInLF :: LF -> [Int]
bInLF (Rel _ _)         = []
bInLF (Eq _  _)         = []

bInLF (Neg lf)          = bInLF lf 
bInLF (Impl lf1 lf2)    = bInLFs [lf1,lf2] 
bInLF (Equi lf1 lf2)    = bInLFs [lf1,lf2]
bInLF (Conj lfs)        = bInLFs lfs 
bInLF (Disj lfs)        = bInLFs lfs 
bInLF (Qt gq abs1 abs2) = bInAs [abs1,abs2] 

bInLFs :: [LF] -> [Int]
bInLFs = nub . concat . map bInLF

bInA :: Abstract -> [Int]
bInA (MkAbstract i lf) = i: bInLF lf

bInAs :: [Abstract] -> [Int]
bInAs = nub . concat . map bInA

freshIndex  :: [LF] -> Int
freshIndex lfs = i+1
  where i      = foldr max 0 (bInLFs lfs)

fresh :: [Term -> LF] -> Int
fresh preds   = freshIndex (map ($ dummy) preds) 
  where dummy = Const ""    

process :: ( String -> [Cat] ) -> String -> [LF]
process lexicon string = map transS (parses lexicon string)

processW :: ( String -> [Cat] ) -> String -> [Abstract]
processW lexicon string = map transWH (parses lexicon string)

