module LogicalForm where

import Parsing
import Model

import Data.Maybe
import Data.List

data Term = Const Entity | Var Int | Struct String [Term]
	deriving (Eq)

data LF = NonProposition
	| Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF] 
        | Forall (Term -> LF)
        | Exists (Term -> LF)
        | Several (Term -> LF)
        | Many (Term -> LF)
        | Most (Term -> LF)
        | WH (Term -> LF)
--	deriving Eq

instance Show Term where
  show (Const name) = show name 
  show (Var i)      = 'x': show i
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

instance Show LF where
	show form = ishow form 1
ishow NonProposition i = "NonProposition"
ishow (Rel r args) i  = r ++ show args
ishow (Eq t1 t2) i    = show t1 ++ "==" ++ show t2
ishow (Neg lf) i      = '~': (ishow lf i)
ishow (Impl lf1 lf2) i = "(" ++ ishow lf1 i ++ "==>" 
		     ++ ishow lf2 i ++ ")"
ishow (Equi lf1 lf2) i = "(" ++ ishow lf1 i ++ "<=>" 
		     ++ ishow lf2 i ++ ")"
ishow (Conj []) i     = "true" 
ishow (Conj lfs) i    = "conj" ++ "[" ++ ishowForms lfs i ++ "]"
ishow (Disj []) i     = "false" 
ishow (Disj lfs) i    = "disj" ++ "[" ++ ishowForms lfs i ++ "]"
ishow (Forall scope) i = "Forall x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))
ishow (Exists scope) i = "Exists x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))
ishow (Several scope) i = "Several x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))
ishow (Many scope) i = "Many x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))
ishow (Most scope) i = "Most x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))
ishow (WH scope) i = "WH x" ++ show i ++ (' ' :
			(ishow (scope (Var i)) (i+1)))

ishowForms :: [LF] -> Int -> String
ishowForms [] _ = ""
ishowForms [f] i = ishow f i
ishowForms (f:fs) i = ishow f i ++ "," ++ ishowForms fs i

transS :: ParseTree Cat Cat -> LF
transS Ep = NonProposition
transS (Branch (Cat _ "S" _ _) [np,vp]) = 
  (transNP np) (transVP vp)

transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "did"    "AUX" _ []),s]) = transS s 
transS (Branch (Cat _ "YN" _ _) 
       [Leaf (Cat "didn't" "AUX" _ []),s]) = transS s

transNP :: ParseTree Cat Cat -> 
                (Term -> LF) -> LF
transNP (Leaf (Cat "#"  "NP" _ _)) = \ p -> p (Var 0)
transNP (Leaf (Cat name "NP" _ _)) = \ p -> p (Const (ided name))
transNP (Branch (Cat _ "NP" _ _) [det,cn]) = 
                             (transDET det) (transCN cn) 

transDET :: ParseTree Cat Cat -> (Term -> LF)
                              -> (Term -> LF)
                              -> LF
transDET (Leaf (Cat "every" "DET" _ _)) = 
  \ p q -> Forall (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "all" "DET" _ _)) = 
  \ p q -> Forall (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "some" "DET" _ _)) = 
  \ p q -> Exists (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "a" "DET" _ _)) = 
  \ p q -> Exists (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "several" "DET" _ _)) = 
  \ p q -> Several (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "no" "DET" _ _)) = 
  \ p q -> Neg (Exists (\v -> Conj [p v, q v]))
transDET (Leaf (Cat "the" "DET" _ _)) = 
  \ p q -> Exists (\v1 -> Conj
  		[Forall (\v2 -> Equi (p v2) (Eq v1 v2)),
		q v1])
transDET (Leaf (Cat "most" "DET" _ _)) = 
  \ p q -> Most (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "many" "DET" _ _)) = 
  \ p q -> Many (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "few" "DET" _ _)) = 
  \ p q -> Neg $ Many (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "which" "DET" _ _)) = 
  \ p q -> WH (\v1 -> Conj
  		[Forall (\v2 -> Equi (p v2) (Eq v1 v2)),
		q v1])

transCN :: ParseTree Cat Cat -> Term -> LF
transCN (Leaf   (Cat name "CN" _ _))          = \ x -> 
                                              Rel name [x]
transCN (Branch (Cat _    "CN" _ _) [cn,rel]) = \ x -> 
                       Conj [transCN cn x, transREL rel x]

transREL :: ParseTree Cat Cat -> Term -> LF
transREL (Branch (Cat _ "COMP" _ _ ) [rel,s]) = 
  \ x -> (transS s)
transREL (Branch (Cat _ "COMP" _ _ ) [s])     = 
  \ x -> (transS s)

transPP :: ParseTree Cat Cat -> (Term -> LF) -> LF
transPP (Leaf   (Cat "#" "PP" _ _)) = \ p -> p (Var 0)
transPP (Branch (Cat _   "PP" _ _) [prep,np]) = transNP np

transVP :: ParseTree Cat Cat -> Term -> LF
transVP (Branch (Cat _ "VP" _ _) [Leaf (Cat name "VP" _ [])]) = 
        \ t -> ( Rel name [t] )
transVP (Branch (Cat _ "VP" _ _) [Leaf (Cat name "VP" _ [_]),obj1]) = 
	case (obj1) of 
		(Branch (Cat _ "PP" _ _) _ ) ->
			\subj -> transPP obj1 (\adv -> Rel name [subj,adv])
		(Leaf (Cat _ "NP" _ _) ) ->
			\subj -> transNP obj1 (\ obj -> Rel name [subj,obj])
transVP (Branch (Cat _ "VP" _ _) [Leaf (Cat name "VP" _ [_,_]),np,obj2]) = 
	case (obj2) of 
		(Branch (Cat _ "PP" _ _) _ ) ->
			\ subj   -> transNP np 
			(\ obj   -> transPP obj2
			 (\ iobj -> Rel name [subj,obj,iobj]))
		(Branch (Cat _ "NP" _ _) _ ) ->
			\ subj   -> transNP np 
			(\ iobj   -> transNP obj2
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

transWH :: ParseTree Cat Cat -> (Term -> LF)
transWH (Branch (Cat _ "WH" _ _ ) [wh,s]) =
	(\v -> Conj [transW wh, transS s])

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


process :: String -> [LF]
process string = map transS (parses string)

-- processW :: String -> [ LF ]
-- processW string = map transWH (parses string)

pick ps x = [ ps !! x ]


--evaluation in model

type Interp a	= String -> [a] -> Bool

int :: Interp Entity
int "man"	= \ [x] -> man x
int "men"	= \ [x] -> man x
int "woman"	= \ [x] -> woman x
int "women"	= \ [x] -> woman x

int "person"	= \ [x] -> people x
int "thing"	= \ [x]	-> things x

int "parent" = \ [x] -> parent x
int "parents" = \ [x] -> parent x
int "mother" = \ [x] -> mother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "boyfriend" = \[x] -> boyfriend x
int "girlfriend" = \[x] -> girlfriend x

int "rock" = \ [x] -> rock x
int "car_window" = \ [x] -> car_window x
int "box" = \ [x] -> box x
int "story" = \ [x] -> story x

int "worked" = \ args -> case args of [x] -> worker x; [x,y] -> worked_with y x
int "work" = int "worked"

int "parented"	= \ [x,y] -> parented y x
int "divorced"	= \ [x,y] -> divorced y x
int "divorce"	= int "divorced"
int "had"	= \ [x,y] -> have y x
int "have"	= \ [x,y] -> have y x
int "contacted"	= \ [x,y] -> contacted y x
int "contact"	= int "contacted"

int "threw"	= \ [x,y,z] ->	throw z y x
int "throw"	= \ [x,y,z] ->	throw z y x
int "gave"	= \ [x,y,z] ->	give z y x
int "give"	= \ [x,y,z] ->	give z y x
int "handed"	= \ [x,y,z] ->	hand z y x
int "hand"	= \ [x,y,z] ->	hand z y x
int "told"	= \ [x,y,z] ->	tell z y x
int "tell"	= \ [x,y,z] ->	tell z y x
int "put" = \ [x,y,z] -> boxed z y x



type FInterp = String -> [Entity] -> Entity

fint :: FInterp
fint name [] =	maybe (entities!!26) id $ lookup name characters

ents = entities
term entity = maybe "NoName" id $ lookup entity names
ided name = maybe Unspec id $ lookup name characters

type TVal = Term -> Entity

lift :: FInterp -> TVal
lift fint (Const a)   = a
lift fint (Struct str ts) =
           fint str (map (lift fint) ts)
lift fint _     = R

term2ent :: Term -> Entity
term2ent (Const a) = a
term2ent _ = R

data Answer = Boolean Bool | Yes | No | NoAnswer
	deriving (Eq)
instance Show Answer where
	show (Boolean bool)	= show bool
	show Yes	= "Yes"
	show No	= "No"
	show NoAnswer	= "NoAnswer"
eval :: LF ->  Answer

eval NonProposition = NoAnswer
eval lf = Boolean $ evl lf


evl (Rel r as)	= int r $ reverse (map term2ent as)
evl (Eq a b)	= a == b
evl (Neg lf)	= not $ evl lf
evl (Impl f1 f2)	= not ( evl f1 && ( not $ evl f2 ) )
evl (Equi f1 f2)	= evl f1 == evl f2
evl (Conj lfs)	= and ( map ( evl ) lfs )
evl (Disj lfs)	= or ( map ( evl ) lfs )
evl (Forall scope)	= and $ testents scope
evl (Exists scope)	= or $ testents scope
evl (Several scope)	= length ( filter id $ testents scope ) < 4
		&& length ( filter id $ testents scope ) > 1
evl (Many scope)	= length ( filter id $ testents scope ) > 5
evl (Most scope)	= length ( filter id $ testents scope ) >
			length ( filter not $ testents scope )

testents :: (Term -> LF) -> [Bool]
testents scope = [ evl (scope (Const e)) | e <- ents ]

passents :: (Term -> LF) -> [Entity]
passents scope = map fst $ filter snd $ zip ents $ testents scope
evalW :: LF -> [Entity]
evalW (WH scope)	= passents  scope

ttest :: (Term -> LF) -> Term -> Bool
ttest scope (Const a) = evl (scope (Const a))
ttest scope _ = evl (scope (Const R))

revttest scope = \x -> not $ evl (scope (Const x))

singleton :: [a] -> Bool
singleton [x]	= True
singleton _	= False

haves = [
	"Did Judy have Kelley?",
	"Did Judy have Rex?",
	"Did Judy have Alice?",
	"Did Judy have a mother?",
	"Did Judy have a son?",
	"Did Judy have a daughter?",
	"Did Kelley have a mother?",
	"Did Rex have a girlfriend?",
	"Did Judy have a boyfriend?",
	"Did Alice have a boyfriend?",
	"Did Alice have a parent?",
	"Did Alice have some parents?",
	"Did Alice have parents?"
	]
wh_questions =[
	"Who worked?",
	"Who did Judy marry?",
	"Who married Judy?",
	"Who gave the box to Judy?",
	"Who gave some box to Judy?",
	"Which person worked?",
	"Which person did Judy marry?",
	"To whom did Rex give some box?",
	"Who did Rex give some box to?"
	]
ungrammatical = [
	"Did Judy worked?",
	"Rex work?",
	"Man worked.",
	"Some man work.",
	"No worked.",
	"No-one work.",
	"Did Judy marry?",
	"Judy marry Rex.",
	"Rex married."
	]
intransitives = [
	"Did Judy work?",
	"Did Rex work?",
	"A man worked.",
	"Some man worked.",
	"No one worked.",
	"No-one worked.",
	"Everybody worked.",
	"Everyone worked.",
	-- "Many persons worked.",
	"No person worked.",
	"Did the man work?",
	"Did some man work?",
	"Did some men work?",
	"Did some woman work?",
	"Did some women work?",
	"Most men worked.",
	"Most men didn't work.",
	"Several men worked.",
	"Several men didn't work.",
	"Many men worked.",
	"Many men didn't work.",
	"All men worked.",
	"No man worked."
	]
transitives = [
	"Did Judy marry Rex?",
	"Judy married Rex.",
	"Rex married Judy.",
	"Some man married Judy.",
	"Some woman married Rex.",
	"Some man helped Alice.",
	"A man helped Alice.",
	"Some woman helped Alice."
	]
ditransitive_tests = [
	"Rex gave some box to Judy.",
	"Did Rex give some box to Judy.",
	"Did Rex give the box to Judy?",
	"Did Rex give the box to someone?",
	"Rex gave several boxes to Judy.",
	"Did someone give something to Judy?",
	"A man gave the box to Judy.",
	"A man gave the box to someone.",
	"A man gave something to someone.",
	"Someone gave something to someone.",

	"Rex gave Judy some box.",
	"Did Rex give Judy some box?",
	"Did Rex give Judy the box?",
	"Did Rex give someone the box?",
	"Rex gave Judy several box.",
	"Did someone give Judy something?",
	"A man gave Judy the box.",
	"A man gave someone the box.",
	"A man gave someone something.",
	"Someone gave someone something."
	]

relclauses = [
	"A man who married Judy worked.",
	"The man who married Judy worked.",
	"Did the man who married Judy work?",
	"Did every man who married Judy work?",
	"The man who gave the box to Judy worked.",
	"Rex divorced the woman that he gave the box to.",
	"Who killed the man that helped the woman " 
	 ++ "that had a boyfriend."
	]


handler core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map core tests )

-- evals tests = putStr $ unlines $ map (\(x,y)->x++show y) $ zip (map (++"\t") tests ) ( map (eval . head . process) tests )

forms tests = putStr $ unlines $ map (\(x,y)->x++show y) $ zip (map (++"\t") tests ) ( map process tests )

lf0 = Rel "worked" [ Const(ents!!17) ]
lf00 = (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] ) 
-- lf000 = (Exists (Conj [(Rel "person" [Var 0]), (Rel "worked" [Var 0]) ] )) (Const(ents)!!17)

lf1 = (Equi  (Rel "married" [ Const(ents!!9), Const(ents!!1) ]) (Neg (Rel "married" [ Const(ents!!8), Const(ents!!17)]) ) )

lf2 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf3 = Rel "married" [ Const (ents !! 8), Const (ents !! 17)]
lf4 = (Impl  (Rel "married" [ Const (ents !! 9), Const        (ents !! 1)]) (Rel "married" [ Const (ents !! 8), Const (ents !!    17)])  )
lf5 = (Conj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )
lf6 = (Disj [ (Rel "married" [ Const (ents !! 9), Const       (ents !! 1)]), (Rel "married" [ Const (ents !! 8), Const (ents !!   17)]) ] )

lf70 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 8)]) ] ) ) (Const (ents !! 12) )
lf71 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf72 = ( \x -> ( Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) ) (Const (ents !! 12) )
lf73 = \x -> Conj [ (Rel "son" [x]), (Rel "have" [x, Const (ents !! 17)]) ]
lf74 = ( \x -> ( Conj [ (Rel "daughter" [x]), (Rel "have" [x, Const (ents !! 17)]) ] ) )
lf75 = \x -> Impl (Rel "son" [x]) (Rel "have" [x, Const (ents !! 17)])
