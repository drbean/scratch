module SolMCWPL where 

import Model
import MCWPL

unspecClose :: [(Entity,Entity)] -> [(Entity,Entity)]
unspecClose r = r ++ [ (Unspec,y) | x <- entities, 
                                    y <- entities,
                                   (x,y) `elem` r ]
                  ++ [ (x,Unspec) | x <- entities,
                                    y <- entities,
                                   (x,y) `elem` r ]

admire = curry (`elem` (unspecClose [(x,G) | x <- entities, person x]))

passivize :: ThreePlacePred -> TwoPlacePred
passivize r = \ x y -> r Unspec x y

