module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	= A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 


--alice, rex, kelley, judy, rock
--                                                :: Entity

characters :: [ (String, Entity) ]

characters = [
	( "alice",	A ),
	( "kelley",	K ),
	( "rex",	X ),
	( "judy",	J ),
	( "rock",	R ),
	( "car_window",	W ),
	( "story",	S ),
	( "box",	B )

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [X]
female	= pred1 [A,K,J]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

people, things :: OnePlacePred

people	= \ x -> (male x || female x )
things	= \ x -> not (people x || x == Unspec)

rock	= pred1 [R]
car_window	= pred1 [W]
box	= pred1 [B]
story	= pred1 [S]
worker	= pred1 [X,J,A]

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

--(parent,child)
parenting	= [ (J,K) ]
--(husband,wife)
marriages	= [ (X,J) ]
--(initiator,wrongdoer?)
divorces	= [ (J,X) ]
--(boyfriend,girlfriend)
unmarried_couples	= [ (X,A) ]
--(contacter,contactee)
contacts	= [ (X,J) ]
coworkers	= [ (X,A) ]
putting	= [ (X,R,B) ]
throwing	= [ (J,R,W) ]
giving	= [ (X,B,J) ]
telling	= [ (J,S,K) ]
boxing	= [ (X,R,B) ]
			
parented, married ::  TwoPlacePred

parented	= pred2 parenting
raised_by	= pred2 $ map swap parenting
married		= pred2 $ marriages ++ map swap marriages
divorced	= pred2 divorces
have	= pred2 $ parenting ++ marriages ++ unmarried_couples
			++ ( map swap $ parenting ++ marriages ++ unmarried_couples )
contacted	= pred2 contacts
worked_with	= pred2 $ coworkers ++ map swap coworkers

man	= male
woman	= female
parent	= pred1 $ map fst parenting
child	= pred1 $ map snd parenting
mother	= \x -> ( female x && parent x )
father	= \x -> ( male x && parent x )
daughter	= \x -> ( female x && child x )
son	= \x -> ( male x && child x )
boyfriend	= pred1 $ map fst unmarried_couples
girlfriend	= pred1 $ map snd unmarried_couples

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

throw, put :: ThreePlacePred

throw	= pred3 throwing
put	= pred3 putting
give	= pred3 giving
hand	= give
tell	= pred3 telling
boxed	= pred3 boxing

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

-- passivize3 :: ThreePlacePred -> OnePlacePred
-- passivize3 r	= \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
