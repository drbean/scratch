module BUStackParser where

import P

type BUStackParser a b = [a] -> [(b,[a],[a])]

type BSPARSER a b = BUStackParser a (ParseTree a b)

infixr 4 <|||>

(<|||>) :: BUStackParser a b -> BUStackParser a b 
		  -> BUStackParser a b 
(p1 <|||> p2) xs = p1 xs ++ p2 xs 

infixl 6 <:::>

(<:::>) :: BUStackParser a b  -> BUStackParser a [b] 
                              -> BUStackParser a [b]
(p <:::> q) xs = [(r:rs,vs++ws,zs) | (r,vs,ys)  <- p xs,
                                 (rs,ws,zs) <- q ys ]

succeedSB :: b -> BUStackParser a b 
succeedSB r xs = [(r,[],xs)]

-- manySB :: BUStackParser a b -> BUStackParser a [b]
-- manySB p = (p <:::> manySB p) <|||> succeedSB []

gapB :: CatLabel -> BSPARSER Cat Cat
gapB c xs = 
	[(Leaf gap, [gap], xs)] where
        gap = Cat "#" c [] []

popB :: Cat -> BSPARSER Cat Cat -> BSPARSER Cat Cat
popB c p xs = 
	[ (t,ws,ys) | (t,w:ws,ys) <- p xs,
                      agree c (t2c t)           ]

leafPSB :: CatLabel -> BSPARSER Cat Cat
leafPSB l []         = [] 
leafPSB l (c:cs) = [(Leaf c,[],cs) | catLabel c == l ]

prsTXTB :: BSPARSER Cat Cat
prsTXTB = conjRB <|||> prsSB

conjRB :: BSPARSER Cat Cat 
conjRB = \ xs -> 
   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], vs++ws, zs) | 
       (s,vs,ys)     <- prsSB xs,
       (conj,[],ys1) <- leafPSB "CONJ" ys, 
       (txt,ws,zs)   <- prsTXTB ys1            ]
-- Should have vs == [] && ws == [].

prsSB :: BSPARSER Cat Cat
prsSB = spRB <|||> cond1RB <|||> cond2RB

spRB :: BSPARSER Cat Cat 
spRB = \ xs -> 
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np',vp],vs++ws,zs) | 
       (np,vs,ys) <- prsNPB xs,
       (vp,ws,zs) <- prsVPB ys, 
        np'       <- assignT Nom np, 
       agreeC np vp,
       subcatList (t2c vp) == [] ]

cond1RB :: BSPARSER Cat Cat 
cond1RB = \ xs -> 
   [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], vs1++ws, zs) | 
       (cond,[],ys) <- leafPSB "COND" xs, 
       (s1,vs1,ys1) <- prsSB ys,
       (s2,ws,zs)   <- prsSB ys1 ]
-- The construction should be an island, so we should have vs1 == [] && ws == []

cond2RB :: BSPARSER Cat Cat 
cond2RB = \ xs -> 
     [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], vs1++ws, zs) | 
         (cond,[],ys) <- leafPSB "COND" xs, 
         (s1,vs1,ys1) <- prsSB ys,
         (_,[],ys2)  <- leafPSB "THEN" ys1, 
         (s2,ws,zs)   <- prsSB ys2 ]
-- Another island construction.  Should have: vs1 == [] && ws == [].

prsNPB :: BSPARSER Cat Cat 
prsNPB = leafPSB "NP" <|||> npRB <|||> gapB "NP" 

npRB :: BSPARSER Cat Cat
npRB = \ xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn], [], zs) | 
      (det,[],ys) <- prsDETB xs, 
      (cn,[],zs)  <- prsCNB ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]
-- No extraction from inside det or cn.  This is not quite right because extraction is OK from inside a pp inside the cn, as in: Who did you see a picture of?

prsDETB :: BSPARSER Cat Cat
prsDETB = leafPSB "DET"

prsCNB :: BSPARSER Cat Cat
prsCNB = leafPSB "CN" <|||> cnrelRB 

prsVPB :: BSPARSER Cat Cat
prsVPB = finVpRB <|||> auxVpRB

vpRB :: BSPARSER Cat Cat
vpRB = \xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),ws,zs) |  
             (vp,[],ys)  <- leafPSB "VP" xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (xps,ws,zs) <- prsNPsorPPsB (length subcatlist) ys, 
             match subcatlist (map t2c xps) ]
-- Since it is always free to parse an empty category, look for a fixed number of categories to match the subcatlist.

finVpRB :: BSPARSER Cat Cat
finVpRB = \xs -> [(vp',vs,ys) | (vp,vs,ys) <- vpRB xs,
                                   vp' <- assignT Tense vp ]

auxVpRB :: BSPARSER Cat Cat
auxVpRB = \xs -> 
     [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
               [aux,inf'], vs++ws, zs) | 
                 (aux,vs,ys) <- prsAUXB xs,
                 (inf,ws,zs) <- vpRB ys,
                  inf'       <- assignT Infl inf ] 

prsAUXB :: BSPARSER Cat Cat
prsAUXB = leafPSB "AUX" <|||> gapB "AUX" 

prsPPB :: BSPARSER Cat Cat
prsPPB = ppRB <|||> gapB "PP" 

ppRB :: BSPARSER Cat Cat
ppRB = \xs -> 
  [ (Branch (Cat "_" "PP" fs []) [prep,np'], vs++ws, zs) | 
      (prep,vs,ys) <- prsPREPB xs, 
      (np,ws,zs)   <- prsNPB ys,
       np'         <- assignT AccOrDat np, 
       fs          <- combine (t2c prep) (t2c np') ]

prsPREPB :: BSPARSER Cat Cat
prsPREPB = leafPSB "PREP"

prsNPorPPB :: BSPARSER Cat Cat
prsNPorPPB = prsNPB <|||> prsPPB

prsNPsorPPsB :: Int -> [Cat] -> [([ParseTree Cat Cat],[Cat],[Cat])]
prsNPsorPPsB i | i <= 0 = succeedSB []
	       | i > 0  = prsNPorPPB <:::> prsNPsorPPsB (i-1)

cnrelRB :: BSPARSER Cat Cat
cnrelRB = \xs -> 
     [ (Branch (Cat "_" "CN" (fs (t2c cn)) []) 
               [cn,rel], vs++ws, zs) |
                 (cn,vs,ys)  <- leafPSB "CN" xs, 
                 (rel,ws,zs) <- prsRELB ys, 
                 agreeC cn rel ]
-- We should enforce ws == [] to treat relative clause as wh-island

prsRELB :: BSPARSER Cat Cat 
prsRELB = relclauseRB <|||> thatlessRB 

relclauseRB :: BSPARSER Cat Cat
relclauseRB = \xs -> 
  [(Branch (Cat "_" "COMP" fs []) [rel,s], ws, zs) |
      (rel,[],ys)  <- leafPSB "REL" xs, 
       fs          <- [fs (t2c rel)],
       gap         <- [Cat "#" "NP" fs []],
       (s,ws,zs) <- popB gap prsSB ys ]
-- Should have ws == [] to treat relative clause as wh-island

thatlessRB :: BSPARSER Cat Cat 
thatlessRB = \ xs -> 
        [ (Branch (Cat "_" "COMP" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [AccOrDat] []], 
           (s,vs,ys) <- popB gap prsSB xs, 
           notElem Wh (fs (t2c s))                       ]

prsYNB :: BSPARSER Cat Cat 
prsYNB = \xs -> 
   [(Branch (Cat "_" "YN" [] []) [aux,s], vs++ws,zs) | 
       (aux,vs,ys) <- prsAUXB xs, 
       gap         <- [Cat "#" "AUX" (fs (t2c aux)) [] ], 
       (s,ws,zs)   <- popB gap prsSB ys ]

-- isWH :: ParseTree Cat Cat -> Bool
-- isWH tr = Wh `elem` (fs (t2c tr))

prsWHB :: BSPARSER Cat Cat 
prsWHB = \xs -> 
   [ (Branch (Cat "_" "WH" [] []) [wh,yn], vs++ws,zs) | 
       (wh,vs,ys) <- prsNPorPPB xs, 
       isWH wh, 
       gapfs      <- [filter (/= Wh) (fs (t2c wh))],
       gap        <- [Cat "#" (catLabel (t2c wh)) gapfs []], 
       (yn,ws,zs) <- popB gap prsYNB ys ]
-- Extraction from a moved wh phrase is impossible so we should have vs == []

parsesB :: String -> [ParseTree Cat Cat]
parsesB str = let ws = lexer str 
             in  [ s | catlist   <- collectCats lexicon ws, 
                       (s,[],[]) <- prsTXTB catlist  
                                 ++ prsYNB catlist   
                                 ++ prsWHB catlist ]

testSuite3 :: [String]
testSuite3 = [ "the boy that Alice smiled admired" ]