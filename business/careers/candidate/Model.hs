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


--rebia, frank, rebia's_brother, albert, mike, jill, jack, food, rings,
--                                                :: Entity

--rebia	= I
--frank	= R
--rebia's_brother	= J
--albert	= L
--mike	= M
--jill	= G
--jack	= A
--food	= F
--rings	= W

characters :: [ (String, Entity) ]

characters = [
	( "rebia",	I ),
	( "frank",	R ),
	( "terry",	V ),
	( "caesar",	J ),
	( "albert",	L ),
	( "mike",	M ),
	( "jill",	G ),
	( "jack",	A ),
	( "food",	F ),
	( "class_ring",	C ),
	( "wedding_ring",	W ),
	( "engagement_ring",	Y )

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [R,J,C,M,A,L,V ]
female	= pred1 [I,G ]

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

class_ring	= pred1 [C]
engagement_ring	= pred1 [Y]
wedding_ring	= pred1 [W]
rings	= \x -> (class_ring x || engagement_ring x || wedding_ring x)
ring	= rings

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

--(parent,child)
parenting	= [ (I,L), (I,M), (R,L), (R,M) ]
--(husband,wife)
marriages	= [ (I,R) ]
--(initiator,wrongdoer?)
divorces	= [ (Unspec,Unspec) ]
--(boyfriend,girlfriend)
unmarried_couples	= [ (A,G) ]
killings	= [ (A,R) ]
walkouts	= [ (R,I) ]
breakups	= walkouts ++ divorces
help	= [ (R,G) ]
approaches	= [ (R,I) ]
wearing	= [ (I,W), (I,Y) ]
siblings	= [
			(I,J), (L,M)
			]
			
giving = [(R,C,I),(R,W,I),(R,Y,I)]
handing = [(I,W,R),(I,Y,R)]

parented, married ::  TwoPlacePred

parented	= pred2 parenting
raised_by	= pred2 $ map swap parenting
married		= pred2 $ marriages ++ map swap marriages
divorced	= pred2 divorces
kill		= pred2 killings
approach		= pred2 approaches
leave		= pred2 walkouts
wear	= pred2 wearing
helped	= pred2 help
have	= pred2 $ parenting ++ marriages ++ unmarried_couples
			++ ( map swap $ parenting ++ marriages ++ unmarried_couples )

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
died	= pred1 $ map snd killings

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

give :: ThreePlacePred

give	= pred3 giving
hand	= pred3 handing

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

-- passivize3 :: ThreePlacePred -> OnePlacePred
-- passivize3 r	= \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
