module LogicalForm where

import Parsing
import Interpretation
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
transNP (Branch (Cat _ "NP" _ _) [det,cn]) = (transDET det) (transCN cn) 

transDET :: ParseTree Cat Cat -> (Term -> LF)
                              -> (Term -> LF)
                              -> LF
transDET (Branch (Cat _ "DET" _ _)
	[Leaf (Cat "'s" "APOS" _ _), Leaf (Cat _ "NP" _ _)]) =
	  \ p q -> Exists (\v -> Conj [p v, q v] )
transDET (Leaf (Cat "every" "DET" _ _)) = 
  \ p q -> Forall (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "all" "DET" _ _)) = 
  \ p q -> Forall (\v -> Impl (p v) (q v) )
transDET (Leaf (Cat "some" "DET" _ _)) = 
  \ p q -> Exists (\v -> Conj [p v, q v] )
transDET (Leaf (Cat "a" "DET" _ _)) = 
  \ p q -> Exists (\v -> Conj [p v, q v] )
transDET (Leaf (Cat "zero" "DET" _ _)) = 
  \ p q -> Exists (\v -> Conj [p v, q v] )
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
		_ ->
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
		(Leaf (Cat _ "NP" _ _) ) ->
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

test_possessives = [
	"Jose's father ate pumpkin_pie.",
	"Did Jose's father eat pumpkin_pie?",
	"Did Jose's brother eat pumpkin_pie?",
	"Did Jose's mother speak English?",
	"Did Jose's father speak English?",
	"Did Jose's brother speak English?",
	"Did Jose's mother speak Spanish?",
	"Did Jose's brother know Spanish?",
	"Did the sister of Jose know Spanish?",
	"Did the father of Jose eat pumpkin_pie?",
	"Did the brother of Jose eat pumpkin_pie?",
	"Did the mother of Jose speak English?",
	"Did the father of Jose speak English?",
	"Did the brother of Jose speak English?",
	"Did the mother of Jose speak Spanish?",
	"Did the brother of Jose know Spanish?",
	"Did the sister of Jose know Spanish?"
	]
haves = [
	"Did Ken have Henry?",
	"Did Ken have John?",
	"Did Ken have a mother?",
	"Did Ken have a son?",
	"Did Ken have a daughter?",
	"Did Henry have a mother?",
	"Did Ken have shoes?",
	"Did Ken have some shoes?",
	"Did Ken have money?",
	"Did Henry have money?",
	"Did John have money?",
	"Did Ken have a parent?",
	"Did Ken have some parents?",
	"Did Ken have parents?",
	"Did Henry have a parent?",
	"Did Henry have some parents?",
	"Did Henry have parents?",
	"Did Ken have work?",
	"Did John have work?",
	"Did Henry have work?"
	]
ungrammatical = [
	"Did John worked?",
	"Ken work?",
	"Man worked.",
	"Some man work.",
	"No worked.",
	"No-one work.",
	"Did John teach?",
	"John teach Ken.",
	"Ken taught."
	]
intransitives = [
	"Did John work?",
	"Did Ken work?",
	"Did Henry work?",
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
	"No man worked.",
	"Did Henry work at a farm?",
	"Henry worked on a farm?",
	"Henry worked in a farm?"
	]
transitives = [
	"Did John study law?",
	"John studied law.",
	"Ken studied law.",
	"Ken studied law at Michigan Law.",
	"Ken studied law at Colorado College",
	"Did Ken go to Colorado College.",
	"Some woman went to Colorado College.",
	"Some man went to Colorado College.",
	"Some boy went to Colorado College.",
	"Some man parented Ken.",
	"A man parented John",
	"Some woman told a story."
	]
ditransitive_tests = [
	"Ken told a story.",
	"Ken told Henry a story.",
	"Ken told a story to Henry.",
	"Ken told a story to John",
	"Ken gave some shoes to John.",
	"Did Ken give some shoes to John.",
	"Did Ken give the shoes to John?",
	"Did Ken give the shoes to someone?",
	"Ken gave several shoes to John.",
	"Did someone give something to John?",
	"A woman gave the shoes to John.",
	"A woman gave the shoes to someone.",
	"A woman gave something to someone.",
	"Someone gave something to someone.",
	"Ken gave John some shoes.",
	"Did Ken give John some shoes?",
	"Did Ken give John the shoes?",
	"Did Ken give someone the shoes?",
	"Ken gave John several shoes.",
	"Did someone give John something?",
	"A man gave John the shoes.",
	"A boy gave John the shoes.",
	"Leroy gave John the shoe.",
	"A man gave someone the shoes.",
	"A man gave someone something.",
	"Someone gave someone something."
	]
wh_questions =[
	"Who worked?",
	"Who did John teach?",
	"Who taught John?",
	"Who gave the shoes to John?",
	"Who gave some shoes to John?",
	"Which person worked?",
	"Which person did John teach?",
	"To whom did Ken give some shoes?",
	"Who did Ken give some shoes to?"
	]
relclauses = [
	"A woman who taught John worked.",
	"The woman who taught John worked.",
	"Did the woman who taught John work?",
	"Did every woman who taught John work?",
	"The woman who gave the shoes to John worked.",
	"Ken divorced the man that she gave the shoes to.",
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
