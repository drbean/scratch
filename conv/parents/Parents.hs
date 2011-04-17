module Parents where 

import MCWPL

-- import FSynF
import Data.List
-- import Data.Tuple
swap	:: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- type LF	= Formula Term

lfSent :: Sent -> LF
lfSent (Sent np vp)	= (lfNP np) (lfVP vp)

data Sent	= Sent NP VP
	deriving Show
data AV	= Hoped | Wanted deriving Show 
data NP	= Sue | Deborah | Dovonna | Don | Robert 
          | Nick | Jock | Eloise | Rod
	  | Johnathan | Brian
	  | Mamie | Suzy | Fay | Jane
          | NP1 DET CN | NP2 DET RCN
	deriving Show
data VP	= VP1 TV NP | VP2 DV NP NP
          deriving Show
data TV	= Bore | Fathered | Adopted | Relinquished
          deriving Show 

data DV	= Gave | Handed deriving Show

su = Struct "Sue" []
deb = Struct "Deborah" []
-- lfNP :: NP -> (Term -> LF) -> LF
lfNP Sue	= \p -> p su
lfNP Deborah	= \p -> p deb
lfNP Dovonna	= \p -> p (Struct "Dovonna" [])
lfNP Don	= \p -> p (Struct "Don" [])
lfNP Robert	= \p -> p (Struct "Robert" [])

lfNP Nick	= \p -> p (Struct "Nick" [])
lfNP Jock	= \p -> p (Struct "Jock" [])
lfNP Eloise	= \p -> p (Struct "Eloise" [])
lfNP Rod	= \p -> p (Struct "Rod" [])

lfNP Johnathan	= \p -> p (Struct "Johnathan" [])
lfNP Brian	= \p -> p (Struct "Brian" [])
lfNP Mamie	= \p -> p (Struct "Mamie" [])
lfNP Suzy	= \p -> p (Struct "Suzy" [])
lfNP Fay	= \p -> p (Struct "Fay" [])
lfNP Jane	= \p -> p (Struct "Jane" [])


lfVP :: VP -> Term -> LF
lfVP (VP1 tv np)	= 
    \ subj -> lfNP np (\ obj -> lfTV tv (subj,obj)) 
lfVP (VP2 dv np1 np2)	=  
    \ subj -> lfNP np1 (\ iobj -> lfNP np2 (\ dobj ->  
                          lfDV dv (subj,iobj,dobj))) 

-- lfTV :: TV -> (Term,Term) -> LF
lfTV Bore	= \ (t1,t2) -> Atom "bear"   [t1,t2]
lfTV Fathered	= \ (t1,t2) -> Atom "beget" [t1,t2]
lfTV Adopted	= \ (t1,t2) -> Atom "adopted" [t1,t2]

-- lfDV :: DV -> (Term,Term,Term) -> LF
lfDV Gave	= \ (t1,t2,t3) -> Atom "give" [t1,t2,t3]
lfDV Handed	= \ (t1,t2,t3) -> Atom "hand" [t1,t2,t3]

-- lfCN :: CN -> Term -> LF
lfCN Woman	= \ t -> Atom "woman"     [t]
lfCN Man	= \ t -> Atom "man"      [t]

-- Model

data Entity	= A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities	=  [minBound..maxBound] 


sue, deborah, dovonna, don,
	nick, jock, eloise, rod,
	johnathan, brian
                                                :: Entity

sue	= S
deborah	= D
dovonna	= O
don	= P
doctor	= C
robert	= R

nick	= N
jock	= J
eloise	= E
rod	= F
margaret= M

johnathan=H
brian	= B
suzy	= S
fay	= F
mamie	= I
jane	= A


man, woman :: OnePlacePred

man	= list2OnePlacePred [N,J,P,R,B]
woman	= list2OnePlacePred [S,D,O,E]
boy	= list2OnePlacePred [H]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

person, thing :: OnePlacePred

person	= \ x -> (man x || woman x || boy x)
thing	= \ x -> not (person x || x == Unspec)

-- parent	= \x -> ( bioparent x || adopter x )
-- mother	= \x -> ( woman x && parent x )

biodad, biomom :: OnePlacePred

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
final_year :: OnePlacePred

final_year	= flip elem [S]

begetting	= [(R,D), (J,N)]
bearing	= [(S,D), (E,N), (M,E), (I,H)]
adopting	= [(O,D), (P,D), (B,H), (A,H) ]
relinquishing	= [(S,D), (A,H)]
abandoning	= [(J,N), (R,D)]
neglecting	= [(I,H)]
-- [(E,N), (F,N), (M,E)] -- birthmother raising
raising	= (adopting ++ begetting ++ bearing) \\
		(relinquishing ++ abandoning ++ neglecting)
giving	= [(S,D,O), (S,D,P)]
handing	= [(S,D,Unspec)]

beget, bear, adopt ::  TwoPlacePred

adopt	= pred2 adopting
adopter	= pred1 $ map fst adopting
adoptee	= pred1 $ map snd adopting

child	= pred2 $ map swap $ begetting ++ bearing ++ raising
-- son	= \x -> ( child x 

beget	= pred2 begetting
biodad	= pred1 $ map fst begetting
father	= \x -> ( man x && ( biodad x || adopter x ) )

bear	= pred2 bearing
biomom	= pred1 $ map fst bearing
mother	= \x -> ( woman x && ( biomom x || adopter x ) )

bioparent	= \x -> ( biomom x || biodad x )
parent	= \x -> ( father x || mother x )

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

hand :: ThreePlacePred

give	= pred3 giving
hand	= pred3 handing

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

-- passivize3 :: ThreePlacePred -> OnePlacePred
-- passivize3 r	= \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

--evaluation in model

type Interp a	= String -> [a] -> Bool

int :: Interp Entity
int "Man"	= \ [x] -> man x
int "Woman"	= \ [x] -> woman x
int "Person"	= \ [x] -> person x
int "Thing"	= \ [x]	-> thing x
int "Adopt"	= \ [x,y]	-> adopt y x
int "Adopter"	= \ [x] -> adopter x
int "Adoptee"	= \ [x] -> adoptee x


int "Beget"	= \ [x,y]	-> beget y x
int "Biodad"	= \ [x] -> biodad x
int "Father"	= \ [x] -> father x

int "Bear"	= \ [x,y]	-> bear y x 
int "Biomom"	= \ [x] -> biomom x
int "Mother"	= \ [x] -> mother x

int "Bioparent"	= \ [x] -> bioparent x
int "Parent"	= \ [x] -> parent x
int "Child"	= \ [x,y]	-> child y x

int "Give"	= \ [x,y,z]	-> give z y x
int "Hand"	= \ [x,y,z] ->	hand z y x


p	= Variable "p" []
q	= Variable "q" []
r	= Variable "r" []

tp, tq, tr :: Term 
tp = Var p
tq = Var q
tr = Var r

ass :: Variable -> Entity
ass	= \v -> E

fint :: FInterp Entity
fint "Sue" [] =	S
fint "Deborah" [] =	D
fint "Dovonna" [] =	O
fint "Don" [] =	P
fint "Doctor" [] =	C
fint "Robert" [] =	R

fint "Nick" [] =	N
fint "Jock" [] =	J
fint "Eloise" [] =	E
fint "Rod" [] =	F
fint "Margaret" [] =	M

fint "Johnathan" [] =	H
fint "Brian" [] =	B

fint "Suzy" [] =	S
fint "Fay" [] =	F
fint "Mamie" [] =	I
fint "Jane" [] =	A

formula3 = Forall p (Forall q (Impl (Atom "Bear" [tp,tq])
                    (Exists r
                      (Conj [Atom "Bear" [tp,tr],
                             Atom "Bear" [tr,tq]]))))

formula4 =  Impl (Atom "Bear" [tp,tq])
                 (Exists r
                   (Conj [Atom "Bear" [tp,tr],
                          Atom "Bear" [tr,tq]]))

formula5 = Forall p ( Forall q formula4 )

