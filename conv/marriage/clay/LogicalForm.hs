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
transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_]),np]) = 
        \ subj -> transNP np (\ obj -> Rel name [subj,obj])

transVP (Branch (Cat _ "VP" _ _) 
                [Leaf (Cat name "VP" _ [_,_]),np,obj2]) = 
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

transWH :: ParseTree Cat Cat -> LF
transWH (Branch (Cat _ "WH" _ _ ) [wh,s]) = case s of
	(Branch (Cat _ "S" _ _ ) [np,vp]) ->
		(transNP np) (transVP vp)
	(Branch (Cat _ "YN" _ _ ) [_,s']) -> Neg (transS s')

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
int "son" = \ [x] -> son x
int "boyfriend" = \[x] -> boyfriend x
int "girlfriend" = \[x] -> girlfriend x

int "rings" = \ [x] -> rings x
int "ring" = \ [x] -> rings x
int "class_ring" = \ [x] -> class_ring x
int "engagement_ring" = \ [x] -> engagement_ring x
int "wedding_ring" = \ [x] -> wedding_ring x

int "died" = \ [x] -> died x
int "die" = \ [x] -> died x

int "married"	= \ [x,y] -> married y x
int "marry"	= \ [x,y] -> married y x
int "parented"	= \ [x,y] -> parented y x
int "divorced"	= \ [x,y] -> divorced y x
int "divorce"	= \ [x,y] -> divorced y x
int "leave"	= \ [x,y] -> leave y x
int "left"	= \ [x,y] -> leave y x
int "killed"	= \ [x,y] -> kill y x
int "kill"	= \ [x,y] -> kill y x
int "approached"	= \ [x,y] -> approach y x
int "approach"	= \ [x,y] -> approach y x
int "put_on"	= \ [x,y] -> wear y x
int "had"	= \ [x,y] -> have y x
int "have"	= \ [x,y] -> have y x
int "helped"	= \ [x,y] -> helped y x

int "gave"	= \ [x,y,z] ->	give z y x
int "give"	= \ [x,y,z] ->	give z y x
int "handed"	= \ [x,y,z] ->	hand z y x
int "hand"	= \ [x,y,z] ->	hand z y x



type FInterp = String -> [Entity] -> Entity

fint :: FInterp
fint name [] =	maybe (entities !! 26) id $ lookup name characters

ents = entities
term entity = maybe "NoName" id $ lookup entity names
ided name = maybe Unspec id $ lookup name characters

type TVal = Term -> Entity

lift :: FInterp -> TVal
lift fint (Const a)   = a
lift fint (Var i)     = error
	"terms with free variable cannot be evaluated"
lift fint (Struct str ts) =
           fint str (map (lift fint) ts)

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


evl (Rel r as)	= int r $ reverse (map (lift fint) as)
evl (Eq a b)	= a == b
evl (Neg lf)	= not $ evl lf
evl (Impl f1 f2)	= not ( evl f1 && ( not $ evl f2 ) )
evl (Equi f1 f2)	= evl f1 == evl f2
evl (Conj lfs)	= and ( map ( evl ) lfs )
evl (Disj lfs)	= or ( map ( evl ) lfs )
evl (Forall scope)	= and $ map (ttest scope) ents
evl (Exists scope)	= or	[evl (scope (Const e))
          		| e <- ents ]
evl (Several scope)	= ( length ( filter (ttest scope) ents ) < 4 )
		&& ( length ( filter (ttest scope) ents ) > 1 )
evl (Many scope)	= ( length ( filter (ttest scope) ents ) > 5 )
evl (Most scope)	= length ( filter (ttest scope) ents ) >
			length ( filter (revttest scope) ents )

evalW :: LF -> [Entity]
evalW (WH scope)	= filter (ttest scope) ents

ttest :: (Term -> LF) -> Entity -> Bool
ttest scope = \x -> evl (scope (Const x))
revttest scope = \x -> not $ evl (scope (Const x))

singleton :: [a] -> Bool
singleton [x]	= True
singleton _	= False

haves = [
	"Did Rebia have Mike?",
	"Did Rebia have Albert?",
	"Did Rebia have Frank?",
	"Did Rebia have Caesar?",
	"Did Rebia have Jack?",
	"Did Rebia have Jill?",
	"Did Rebia have a mother?",
	"Did Rebia have a son?",
	"Did Rebia have a daughter?",
	"Did Mike have a mother?",
	"Did Albert have a father?",
	"Did Jack have a girlfriend?",
	"Did Frank have a girlfriend?",
	"Did Rebia have a boyfriend?",
	"Did Jill have a boyfriend?",
	"Did Albert have a parent?",
	"Did Albert have some parents?",
	"Did Albert have parents?"
	]
wh_questions =[
	"Who died?",
	"Who did Rebia marry?",
	"Who married Rebia?",
	"Who gave the rings to Rebia?",
	"Who did Frank give some rings to?",
	"To whom did Frank give some rings?",
	"Which person died?",
	"Which person did Rebia marry?"
	]
ungrammatical = [
	"Did Rebia died?",
	"Frank die?",
	"Man died.",
	"Some man die.",
	"No died.",
	"No-one die.",
	"Did Rebia marry?",
	"Rebia marry Frank.",
	"Frank married."
	]
intransitives = [
	"Did Rebia die?",
	"Did Frank die?",
	"A man died.",
	"Some man died.",
	"No one died.",
	"No-one died.",
	"Everybody died.",
	"Everyone died.",
	-- "Many persons died.",
	"No person died.",
	"Did the man die?",
	"Did some man die?",
	"Did some men die?",
	"Did some woman die?",
	"Did some women die?",
	"Most men died.",
	"Most men didn't die.",
	"Several men died.",
	"Several men didn't die.",
	"Many men died.",
	"Many men didn't die.",
	"All men died.",
	"No man died."
	]
transitives = [
	"Did Rebia marry Frank?",
	"Rebia married Frank.",
	"Frank married Rebia.",
	"Some man married Rebia.",
	"Some woman married Frank.",
	"Some man helped Jill.",
	"A man helped Jill.",
	"Some woman helped Jill."
	]
ditransitive_tests = [
	"Frank gave some rings to Rebia.",
	"Did Frank give some rings to Rebia.",
	"Did Frank give the rings to Rebia?",
	"Did Frank give the rings to someone?",
	"Frank gave several rings to Rebia.",
	"Did someone give something to Rebia?",
	"A man gave the rings to Rebia.",
	"A man gave the rings to someone.",
	"A man gave something to someone.",
	"Someone gave something to someone.",

	"Frank gave Rebia some rings.",
	"Did Frank give Rebia some rings?",
	"Did Frank give Rebia the rings?",
	"Did Frank give someone the rings?",
	"Frank gave Rebia several rings.",
	"Did someone give Rebia something?",
	"A man gave Rebia the rings.",
	"A man gave someone the rings.",
	"A man gave someone something.",
	"Someone gave someone something."
	]

relclauses = [
	"The man who married Rebia died.",
	"Did the man who married Rebia die?",
	"Did every man who married Rebia die?",
	"The man who gave the rings to Rebia died.",
	"Frank left the woman that he gave the rings to.",
	"Who killed the man that helped the woman " 
	 ++ "that had a boyfriend."
	]


handler core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map core tests )

-- evals tests = putStr $ unlines $ map (\(x,y)->x++show y) $ zip (map (++"\t") tests ) ( map (eval . head . process) tests )

forms tests = putStr $ unlines $ map (\(x,y)->x++show y) $ zip (map (++"\t") tests ) ( map process tests )

lf0 = Rel "died" [ Const(ents!!17) ]
lf00 = (Conj [(Rel "person" [Var 0]), (Rel "died" [Var 0]) ] ) 
-- lf000 = (Exists (Conj [(Rel "person" [Var 0]), (Rel "died" [Var 0]) ] )) (Const(ents)!!17)

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
