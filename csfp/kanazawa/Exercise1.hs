module Exercise1 where

import Data.List
import FSynF
import MCWPL

some_boy_helped_some_girl :: Int -> Sent
some_boy_helped_some_girl i = 
	Sent (NP1 Some Boy) (VP1 Helped (some_girl i))

some_girl :: Int -> NP
some_girl i = 
	if i <= 0 then NP1 Some Girl
	else NP2 Some (RCN1 Girl That (VP1 Helped (some_girl (i-1))))

lfSentTD :: Sent -> LF
lfSentTD s = lfSentInt s 1

lfSentInt :: Sent -> Int -> LF
lfSentInt (Sent np vp) i = lfNPInt np i (lfVPInt vp)

lfNPInt :: NP -> Int -> (Int -> Term -> LF) -> LF
lfNPInt (NP1 det cn)  i = lfDETInt det i (\ j -> lfCN cn) 
lfNPInt (NP2 det rcn) i = lfDETInt det i (lfRCNInt rcn)
lfNPInt np            i = \ p -> (lfNP np) (p i)
-- the case of np = Someone or Everyone is missing in MCWPL.hs

lfVPInt :: VP -> Int -> Term -> LF
lfVPInt (VP1 tv np)      i =
    \ subj -> lfNPInt np i (\ j -> \ obj -> lfTV tv (subj,obj))
lfVPInt (VP2 dv np1 np2) i = 
    \ subj -> lfNPInt np1 i (\ j -> \ iobj -> lfNPInt np2 j (\ k -> \ dobj -> 
                          lfDV dv (subj,iobj,dobj)))
lfVPInt vp               _ = lfVP vp

lfDETInt :: DET -> Int -> (Int -> Term -> LF) -> (Int -> Term -> LF) -> LF
lfDETInt Some  i p q = Exists v (Conj [p j (Var v), q j (Var v)]) 
        where
        v = Variable "x" [i]
        j = i+1
lfDETInt Every i p q = Forall v (Impl (p j (Var v)) (q j (Var v))) 
        where
        v = Variable "x" [i]
        j = i+1
lfDETInt No    i p q = Neg (Exists v (Conj [p j (Var v), q j (Var v)]))
        where
        v = Variable "x" [i]
        j = i+1
lfDETInt The   i p q = Exists v1 (Conj 
                 [Forall v2 (Equi (p (i+2) (Var v2)) 
                                  (Eq (Var v1) (Var v2))), 
                  q (i+1) (Var v1)])
      where
           v1 = Variable "x" [i]
           v2 = Variable "x" [i+1]

lfRCNInt :: RCN -> Int -> Term -> LF
lfRCNInt (RCN1 cn _ vp)    i = \ t -> Conj [lfCN cn t, lfVPInt vp i t]
lfRCNInt (RCN2 cn _ np tv) i = \ t -> Conj [lfCN cn t, 
                       lfNPInt np i (\j -> \ subj -> lfTV tv (subj,t))]

lfSentBU :: Sent -> LF
lfSentBU s = fst (lfIntSent s)

lfIntSent :: Sent -> (LF, Int)
lfIntSent (Sent np vp) = (q p, i)
        where (q, i) = lfIntNP np j
              (p, j) = lfIntVP vp

lfIntNP :: NP -> Int -> ((Term -> LF) -> LF, Int)
lfIntNP (NP1 det cn)  i = (q (lfCN cn), j)
        where (q, j) = lfIntDET det i 0
lfIntNP (NP2 det rcn) i = (q p, k)
    where
        (q, k) = lfIntDET det i j
        (p, j) = lfIntRCN rcn
lfIntNP np            i = (lfNP np, i)
-- the case of np = Someone or Everyone is missing in MCWPL.hs

lfIntVP :: VP -> (Term -> LF, Int)
lfIntVP (VP1 tv np) = (p, i)
        where
        p = \ subj -> q (\ obj -> lfTV tv (subj,obj))
        (q, i) = lfIntNP np 0
lfIntVP (VP2 dv np1 np2) = (p, i)
        where
        p = \ subj -> q1 (\ iobj -> q2 (\ dobj -> (lfDV dv (subj,iobj,dobj))))
        (q2, j) = lfIntNP np2 0
        (q1, i) = lfIntNP np1 j
lfIntVP vp = (lfVP vp, 0)

lfIntDET :: DET -> Int -> Int -> ((Term -> LF) -> (Term -> LF) -> LF, Int)
lfIntDET Some  i j = (\ p q -> Exists v (Conj [p (Var v), q (Var v)]), k+1)
    where
        v = Variable "x" [k+1]
        k = (max i j)
lfIntDET Every i j = (\ p q -> Forall v (Impl (p (Var v)) (q (Var v))), k+1)
    where
        v = Variable "x" [k+1]
        k = (max i j)
lfIntDET No    i j = (\ p q -> Neg (Exists v (Conj [p (Var v), q(Var v)])), k+1)
    where
        v = Variable "x" [k+1]
        k = max i j
lfIntDET The   i j = (\ p q -> Exists v2 (Conj
                      [Forall v1 (Equi (p (Var v1)) (Eq (Var v2) (Var v1))),
                      q (Var v2)]),
                      k+2)
    where
        v1 = Variable "x" [k+1]
        v2 = Variable "x" [k+2]
        k = max i j

lfIntRCN :: RCN -> (Term -> LF, Int)
lfIntRCN (RCN1 cn _ vp)    = (\ t -> Conj [lfCN cn t, p t], i)
    where
        (p, i) = lfIntVP vp
lfIntRCN (RCN2 cn _ np tv) = (\ t -> Conj [lfCN cn t, 
                                           q (\ subj -> lfTV tv (subj, t))],
                              i)
    where
        (q, i) = lfIntNP np 0
