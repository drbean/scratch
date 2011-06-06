module Sentencing where 

import Data.List
-- import Data.Tuple
swap	:: (a,b) -> (b,a)
swap (a,b) = (b,a)

data Formula a = Atom String [a]
               | Eq a a
               | Neg  (Formula a)
               | Impl (Formula a) (Formula a) 
               | Equi (Formula a) (Formula a)
               | Conj [Formula a]
               | Disj [Formula a] 
               | Forall Variable (Formula a)
               | Exists Variable (Formula a)
               deriving (Eq,Show)

--instance Show a => Show (Formula a) where 
--  show (Atom s [])   = s
--  show (Atom s xs)   = s ++ show xs 
--  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2
--  show (Neg form)    = '~' : (show form)
--  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
--                           ++ show f2 ++ ")"
--  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>" 
--                           ++ show f2 ++ ")"
--  show (Conj [])     = "true" 
--  show (Conj fs)     = "conj" ++ show fs 
--  show (Disj [])     = "false" 
--  show (Disj fs)     = "disj" ++ show fs 
--  show (Forall v f)  = "A " ++  show v ++ (' ' : show f)
--  show (Exists v f)  = "E " ++  show v ++ (' ' : show f)

formula0 = Atom "R" [x,y]
formula1 = Forall x (Atom "R" [x,x])
formula2 = Forall x 
            (Forall y
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))

data Term = Var Variable | Struct String [Term] 
            deriving (Eq,Ord)

instance Show Term where 
  show (Var v)       = show v 
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

varsInTerm :: Term -> [Variable]
varsInTerm (Var v)       = [v]
varsInTerm (Struct s ts) = varsInTerms ts

varsInTerms :: [Term] -> [Variable]
varsInTerms = nub . concat . map varsInTerm

type LF	= Formula Term

lfSent :: Sent -> LF
lfSent (Sent np vp)	= (lfNP np) (lfVP vp)

data Sent = Sent NP VP deriving Show
data NP   = Ms_Ma | Dred | Doug | Mort | Foster | Memorial
		| Robert | Sam |David | Robert's_Ghost
          | NP1 DET CN | NP2 DET RCN 
          deriving Show
data DET  = The | Every | Some | No | Most
          deriving Show
data CN   = Man | Woman | Mother   | Boy   | Undertaker | Gravedigger | Doctor
		| Sexton
		| Hospital | Cemetery
		| Ghost | Ghost_hunter
          deriving Show 
data ADJ  = Fake deriving Show
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          | RCN3 ADJ CN
          deriving Show
data That = That deriving Show
data VP   = Died | Cried | Sighed
          | VP1 TV NP | VP2 DV NP NP
          | VP3 AV To INF
          deriving Show 
data TV   = Loved | Hired | Helped
		| Frightened | Believed
		| Dug | Sold | Killed | Buried | Owned | Saw
          deriving Show 

data DV   = Handed deriving Show
data AV   = Hoped | Wanted deriving Show 
data INF  = Sigh | Cry | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch 
            deriving Show 
data To   = To deriving Show

type Name     = String 
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq,Ord)

instance Show Variable where 
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = "" 
           showInts [i]    = show i  
           showInts (i:is) = show i ++ "_" ++ showInts is

lfNP :: NP -> (Term -> LF) -> LF
lfNP Ms_Ma	= \p -> p (Struct "Ms_Ma" [])
lfNP Dred	= \p -> p (Struct "Dred" [])
lfNP Doug	= \p -> p (Struct "Doug" [])
lfNP Mort	= \p -> p (Struct "Mort" [])
lfNP Foster	= \p -> p (Struct "Foster" [])

lfNP Robert	= \p -> p (Struct "Robert" [])
lfNP Sam	= \p -> p (Struct "Sam" [])
lfNP David	= \p -> p (Struct "David" [])
lfNP Robert's_Ghost	= \p -> p (Struct "Robert's_Ghost" [])

lfNP (NP1 det cn)  = (lfDET det) (lfCN cn) 
lfNP (NP2 det rcn) = (lfDET det) (lfRCN rcn) 

lfVP :: VP -> Term -> LF

lfVP Sighed   = \ t -> Atom "sigh"   [t]
lfVP Cried   = \ t -> Atom "cry"   [t]
lfVP Died   = \ t -> Atom "die"   [t]

lfVP (VP1 tv np) =
    \ subj -> lfNP np (\ obj -> lfTV tv (subj,obj))
lfVP (VP2 dv np1 np2) = 
    \ subj -> lfNP np1 (\ iobj -> lfNP np2 (\ dobj -> 
                          lfDV dv (subj,iobj,dobj)))

lfTV :: TV -> (Term,Term) -> LF
lfTV Hired	= \ (t1,t2) -> Atom "hire"   [t1,t2]
lfTV Helped	= \ (t1,t2) -> Atom "help"   [t1,t2]
lfTV Frightened	= \ (t1,t2) -> Atom "frighten"   [t1,t2]
lfTV Saw	= \ (t1,t2) -> Atom "see"   [t1,t2]
lfTV Believed	= \ (t1,t2) -> Atom "believe"   [t1,t2]
lfTV Killed	= \ (t1,t2) -> Atom "kill"   [t1,t2]
lfTV Buried	= \ (t1,t2) -> Atom "bury"   [t1,t2]
lfTV Owned	= \ (t1,t2) -> Atom "own"   [t1,t2]
lfTV Dug	= \ (t1,t2) -> Atom "dig"   [t1,t2]
lfTV Sold	= \ (t1,t2) -> Atom "sell"   [t1,t2]

lfDV :: DV -> (Term,Term,Term) -> LF
lfDV Handed	= \ (t1,t2,t3) -> Atom "hand" [t1,t2,t3]

lfCN :: CN -> Term -> LF
lfCN Woman	= \t -> Atom "woman"	[t]
lfCN Man	= \t -> Atom "man"	[t]
lfCN Doctor	= \t -> Atom "doctor"	[t]
lfCN Sexton	= \t -> Atom "sexton" [t]

lfCN Hospital	= \t -> Atom "hospital" [t]
lfCN Cemetery	= \t -> Atom "cemetery" [t]


lfDET :: DET -> (Term -> LF) -> (Term -> LF) -> LF

bInLF :: LF -> [Int]
bInLF (Atom _ _)                  = []
bInLF (Eq _ _)                    = []
bInLF (Neg lf)                    = bInLF lf 
bInLF (Impl lf1 lf2)              = bInLFs [lf1,lf2] 
bInLF (Equi lf1 lf2)              = bInLFs [lf1,lf2]

bInLF (Conj lfs)                  = bInLFs lfs 
bInLF (Disj lfs)                  = bInLFs lfs 
bInLF (Forall (Variable _ [i]) f) = i : bInLF f 
bInLF (Exists (Variable _ [i]) f) = i : bInLF f 

bInLFs :: [LF] -> [Int]
bInLFs = nub . concat . map bInLF

freshIndex  :: [LF] -> Int
freshIndex lfs = i+1
       where i = maximum (0:(bInLFs lfs))

fresh :: [Term -> LF] -> Int
fresh preds   = freshIndex (map ($ dummy) preds) 
  where dummy = Struct "" []

lfDET Some  p q = Exists v (Conj [p (Var v), q (Var v)]) 
        where v = Variable "x" [fresh[p,q]]
lfDET Every p q = Forall v (Impl (p (Var v)) (q (Var v))) 
        where v = Variable "x" [fresh[p,q]]
lfDET No    p q = Neg (Exists v (Conj [p (Var v),q (Var v)]))
        where v = Variable "x" [fresh[p,q]]

lfDET The p q = Exists v1 (Conj 
                 [Forall v2 (Equi (p (Var v2)) 
                                  (Eq (Var v1) (Var v2))), 
                  q (Var v1)])
      where
           i  = fresh[p,q]
           v1 = Variable "x" [i]
           v2 = Variable "x" [i+1]

lfRCN :: RCN -> Term -> LF
lfRCN (RCN1 cn _ vp)    = \ t -> Conj [lfCN cn t, lfVP vp t]
lfRCN (RCN2 cn _ np tv) = \ t -> Conj [lfCN cn t, 
                       lfNP np (\ subj -> lfTV tv (subj,t))]

lf1 = lfSent (Sent (NP1 Some Hospital) (VP1 Helped (NP1 Some Woman))) 
lf2 = lfSent (Sent (NP2 The (RCN2 Sexton That Mort Hired)) Sighed) 
lf3 = lfSent (Sent (NP2 The (RCN1 Woman That (VP1 Hired Mort))) Cried)

-- Model

data Entity	= A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities	=  [minBound..maxBound] 


mort, dred, doug, ma, foster,
	robert, sam, david, robert's_ghost,
	don, mike, philip
                                                :: Entity

mort	= T
dred	= E
doug	= U
ma	= A
foster	= F
memorial= H
oakland	= O

sam	= S
robert	= R
david	= D
robert's_ghost	= G

philip	= P
mike	= M
don	= N

man, woman :: OnePlacePred

man	= list2OnePlacePred [T,E,U,S,R,D,M,N]
woman	= list2OnePlacePred [A]
cemetery= list2OnePlacePred [O]
hospital= list2OnePlacePred [H]
sexton	= pred1 [U,S]
grave	= pred1 [G]

sigh	= pred1 [U]
cry	= pred1 [A]
die	= pred1 [P,R]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

person, thing :: OnePlacePred

person	= \ x -> (man x || woman x )
thing	= \ x -> not (person x || x == Unspec)

working = [M,U,F, S]
studying = [D]

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

work :: OnePlacePred
work = pred1 working

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

parenting	= [ (A,D)]
loving	= parenting
hiring	= [(A,M), (M,U)]
helping = [(O,A)]
burying = [(U,E), (Unspec, P), (Unspec, R)]

handing = [(O,D,M)]

hire, help, bury ::  TwoPlacePred

hire	= pred2 hiring
help	= pred2 helping
bury	= pred2 burying

parent	= pred1 $ map fst parenting
mother	= \x -> ( woman x && parent x )

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

hand :: ThreePlacePred

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
int "Hospital" = \ [x] -> hospital x
int "Grave" = \ [x] -> grave x

int "Sexton" = \ [x] -> sexton x


int "Parent" = \ [x] -> parent x
int "Mother" = \ [x] -> mother x

int "Cry" = \ [x] -> cry x
int "Die" = \ [x] -> die x
int "Sigh" = \ [x] -> sigh x

int "Hand"	= \ [x,y,z] ->	hand z y x
int "Hire" = \ [x,y] -> hire y x



x	= Variable "x" []
y	= Variable "y" []
z	= Variable "z" []

tx, ty, tz :: Term 
tx = Var x
ty = Var y
tz = Var z

ass :: Variable -> Entity
ass	= \v -> E

fint :: FInterp Entity
fint "Dred" [] =	E
fint "Mort" [] =	T
fint "Ms_Ma" [] =	A
fint "Doug" [] =	U
fint "Foster" [] =	F
fint "Oakland" [] =	O
fint "Memorial" [] =	H

fint "Robert" [] =	R
fint "Sam" [] =		S
fint "David" [] =	D
fint "Robert's_Ghost" [] =	G

fint "Philip" [] =	P
fint "Mike" [] =	M
fint "Don" [] =		N


type Lookup a = Variable -> a

change :: Lookup a -> Variable -> a -> Lookup a 
change g x d = \ v -> if x == v then d else g v

type FInterp a = String -> [a] -> a

zero = Struct "zero" []

fint1 :: FInterp Int 
fint1 "zero"  []    = 0
fint1 "s"     [i]   = succ i
fint1 "plus"  [i,j] = i + j 
fint1 "times" [i,j] = i * j 

type TVal a = Term -> a

liftLookup :: FInterp a -> Lookup a -> TVal a 
liftLookup fint g (Var v)         = g v
liftLookup fint g (Struct str ts) = 
           fint str (map (liftLookup fint g) ts)

evl :: Eq a => 
  [a]          -> 
  Interp  a    -> 
  FInterp a    -> 
  Lookup  a    -> 
  Formula Term -> Bool

evl domain i fint = evl' where 
   lift = liftLookup fint 
   evl' g (Atom str ts) = i str (map (lift g) ts)
   evl' g (Eq   t1 t2)  = lift g t1 == lift g t2
   evl' g (Neg  f)      = not (evl' g f)
   evl' g (Impl f1 f2)  = not ((evl' g f1) && 
                               not (evl' g f2))
   evl' g (Equi f1 f2)  = evl' g f1 == evl' g f2
   evl' g (Conj fs)     = and (map (evl' g) fs)
   evl' g (Disj fs)     = or  (map (evl' g) fs)
   evl' g (Forall v f)  = and [ evl' (change g v d) f | 
                                d <- domain ]
   evl' g (Exists v f)  = or  [ evl' (change g v d) f | 
                                d <- domain ]

formula3 = Forall x (Forall y (Impl (Atom "R" [tx,ty])
                    (Exists z
                      (Conj [Atom "R" [tx,tz],
                             Atom "R" [tz,ty]]))))

formula4 =  Impl (Atom "R" [tx,ty])
                 (Exists z
                   (Conj [Atom "R" [tx,tz],
                          Atom "R" [tz,ty]]))

formula5 = Forall x ( Forall y formula4 )

formula6 = 
	(Exists x (Conj [Atom "Woman" [tx], Atom "Cry" [tx] ] ) )

checkForm :: Formula Term -> Bool
checkForm f = evl entities int fint (\(Variable _ _) -> A) f

checkSentence :: Sent -> Bool

checkSentence s = evl entities int fint (\ (Variable _ _) -> A) ( lfSent s )
