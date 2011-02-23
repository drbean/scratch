module SolP where 

import P

immdominance :: ParseTree a b -> Rel Pos
immdominance t = [ (p,q) | (p,q) <- properdominance t,
                           not (any (inbetween p q) (pos t)) ]
  where inbetween p q r =  (p,r) `elem` (properdominance t)
                        && (r,q) `elem` (properdominance t)

mutualcCommand :: ParseTree a b -> Rel Pos
mutualcCommand t = [ (p,q) | (p,q) <- cCommand t,
                             (q,p) `elem` cCommand t ]

immprecedence :: ParseTree a b -> Rel Pos
immprecedence t = [ (p,q) | (p,q) <- precedence t,
                            not (any (inbetween p q) (pos t)) ]
  where inbetween p q r =  (p,r) `elem` (precedence t)
                        && (r,q) `elem` (precedence t)

command :: ParseTree a b -> Rel Pos
command t = [ (p,q) | p <- pos t,
                      q <- pos t,
                      (p,q) `notElem` (dominance t),
                      (q,p) `notElem` (dominance t),
                      ([],p)   `elem` (dominance t),
                      ([],q)   `elem` (dominance t) ]

