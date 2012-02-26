module MCWPL_HOAS where 

import Data.List
import Syntax_HOAS
import Model

type LF = Formula Entity
type LFTerm = Term Entity

lfSent :: Sent -> LF
lfSent (Sent np vp) = (lfNP np) (lfVP vp)

lfNP :: NP -> (LFTerm -> LF) -> LF
lfNP SnowWhite     = \ p -> p (Struct "SnowWhite"  [])
lfNP Alice         = \ p -> p (Struct "Alice"      [])
lfNP Dorothy       = \ p -> p (Struct "Dorothy"    [])
lfNP Goldilocks    = \ p -> p (Struct "Goldilocks" [])
lfNP LittleMook    = \ p -> p (Struct "LittleMook" [])
lfNP Atreyu        = \ p -> p (Struct "Atreyu"     [])
lfNP (NP1 det cn)  = (lfDET det) (lfCN cn) 
lfNP (NP2 det rcn) = (lfDET det) (lfRCN rcn) 

lfVP :: VP -> LFTerm -> LF
lfVP Laughed   = \ t -> Atom "laugh"   [t]
lfVP Cheered   = \ t -> Atom "cheer"   [t]
lfVP Shuddered = \ t -> Atom "shudder" [t]

lfVP (VP1 tv np) =
    \ subj -> lfNP np (\ obj -> lfTV tv (subj,obj))
lfVP (VP2 dv np1 np2) = 
    \ subj -> lfNP np1 (\ iobj -> lfNP np2 (\ dobj -> 
                          lfDV dv (subj,iobj,dobj)))

lfTV :: TV -> (LFTerm,LFTerm) -> LF
lfTV Loved    = \ (t1,t2) -> Atom "love"   [t1,t2]
lfTV Admired  = \ (t1,t2) -> Atom "admire" [t1,t2]
lfTV Helped   = \ (t1,t2) -> Atom "help"   [t1,t2]
lfTV Defeated = \ (t1,t2) -> Atom "defeat" [t1,t2]

lfDV :: DV -> (LFTerm,LFTerm,LFTerm) -> LF
lfDV Gave = \ (t1,t2,t3) -> Atom "give" [t1,t2,t3]

lfCN :: CN -> LFTerm -> LF
lfCN Girl     = \ t -> Atom "girl"     [t]
lfCN Boy      = \ t -> Atom "boy"      [t]

lfCN Princess = \ t -> Atom "princess" [t] 
lfCN Dwarf    = \ t -> Atom "dwarf"    [t] 
lfCN Giant    = \ t -> Atom "giant"    [t] 
lfCN Wizard   = \ t -> Atom "wizard"   [t] 
lfCN Sword    = \ t -> Atom "sword"    [t] 
lfCN Dagger   = \ t -> Atom "dagger"   [t] 

lfDET :: DET -> (LFTerm -> LF) -> (LFTerm -> LF) -> LF

lfDET Some  p q = Exists (\ v -> Conj [p v, q v]) 
lfDET Every p q = Forall (\ v -> Impl (p v) (q v)) 
lfDET No    p q = Neg (Exists (\v -> Conj [p v,q v]))

lfDET The p q = Exists (\ v1 -> Conj 
                 [Forall (\ v2 -> Equi (p v2) 
                                  (Eq v1 v2)), 
                  q v1])

lfRCN :: RCN -> LFTerm -> LF
lfRCN (RCN1 cn _ vp)    = \ t -> Conj [lfCN cn t, lfVP vp t]
lfRCN (RCN2 cn _ np tv) = \ t -> Conj [lfCN cn t, 
                       lfNP np (\ subj -> lfTV tv (subj,t))]

lf1 = lfSent (Sent (NP1 Some Dwarf) 
                   (VP1 Defeated (NP1 Some Giant))) 
lf2 = lfSent (Sent (NP2 The (RCN2 Wizard 
                                  That Dorothy Admired)) 
                    Laughed) 
lf3 = lfSent (Sent (NP2 The (RCN1 Princess 
                                  That (VP1 Helped Alice))) 
                    Shuddered)

type Interp a = String -> [a] -> Bool

int0 :: Interp Entity
int0 "laugh"   = \ [x]     -> laugh x
int0 "cheer"   = \ [x]     -> cheer x
int0 "shudder" = \ [x]     -> shudder x
int0 "love"    = \ [x,y]   -> love y x 
int0 "admire"  = \ [x,y]   -> admire y x
int0 "help"    = \ [x,y]   -> help y x
int0 "defeat"  = \ [x,y]   -> defeat y x
int0 "give"    = \ [x,y,z] -> give z y x

int0 "girl"    = \ [x]     -> girl x
int0 "boy"     = \ [x]     -> boy x
int0 "princess"     = \ [x]     -> princess x
int0 "dwarf"     = \ [x]     -> dwarf x
int0 "giant"     = \ [x]     -> giant x
int0 "wizard"     = \ [x]     -> wizard x
int0 "sward"     = \ [x]     -> sword x
int0 "dagger"     = \ [x]     -> dagger x


fint0 "SnowWhite" []  = snowWhite
fint0 "Alice" []      = alice
fint0 "Dorothy" []    = dorothy
fint0 "Goldilocks" [] = goldilocks
fint0 "LittleMook" [] = littleMook
fint0 "Atreyu" []     = atreyu


int1 :: String -> [Int] -> Bool
int1 "R" = rconvert (<)
     where 
           rconvert :: (a -> a -> Bool) -> [a] -> Bool
           rconvert r [x,y] = r x y 

type FInterp a = String -> [a] -> a

zero = Struct "zero" []

fint1 :: FInterp Int 
fint1 "zero"  []    = 0
fint1 "s"     [i]   = succ i
fint1 "plus"  [i,j] = i + j 
fint1 "times" [i,j] = i * j 

type TVal a = Term a -> a

lift :: (Bounded a) => FInterp a -> TVal a 
lift fint (Param param)   = param
lift fint (Var i)         = error "terms with free variables cannot be evaluated"
lift fint (Struct str ts) = 
           fint str (map (lift fint) ts)

evl :: Eq a => Bounded a =>
  [a]          -> 
  Interp  a    -> 
  FInterp a    -> 
  Formula a    -> Bool

evl domain i fint = evl' where 
   evl' (Atom str ts) = i str (map (lift fint) ts)
   evl' (Eq   t1 t2)  = lift fint t1 == lift fint t2
   evl' (Neg  f)      = not (evl' f)
   evl' (Impl f1 f2)  = not ((evl' f1) && 
                               not (evl' f2))
   evl' (Equi f1 f2)  = evl' f1 == evl' f2
   evl' (Conj fs)     = and (map evl' fs)
   evl' (Disj fs)     = or  (map evl' fs)
   evl' (Forall scope)  = and [ evl' (scope (Param d)) | 
                                d <- domain ]
   evl' (Exists scope)  = or  [ evl' (scope (Param d)) | 
                                d <- domain ]

formula3 = Forall (\ x -> Forall (\ y -> Impl (Atom "R" [x,y])
                    (Exists (\ z ->
                      (Conj [Atom "R" [x,z],
                             Atom "R" [z,y]])))))

formula4 =  Impl (Atom "R" [tx,ty])
                 (Exists (\ z -> 
                   (Conj [Atom "R" [tx,z],
                          Atom "R" [z,ty]])))

int3 :: String -> [Entity] -> Bool
int3 "R" = \ [x,y] -> defeat y x

fint2 :: String -> [Entity] -> Entity
fint2 "zero" [] = A

