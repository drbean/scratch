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


characters :: [ (String, Entity) ]

characters = [
	( "barbara",	B ),
	( "tadeusz",	T ),
	( "eva",	E ),
	( "fast_track",	F ),
	( "high_school",	H),
	( "college",	U)

	]

names :: [( Entity, String )]

names = map swap characters

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

male, female :: OnePlacePred
male	= pred1 [T]
female	= pred1 [B,E]
company	= pred1 [F]
college	= pred1 [U]
high_school	= pred1 [H]

people, things :: OnePlacePred
people	= \ x -> (male x || female x )
things	= \ x -> not (people x || x == Unspec)

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

--(person,school)
education	= [ (B,H), (T,U), (E,U) ]

graduated_from ::  TwoPlacePred
graduated_from	= pred2 education

man	= male
woman	= female

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

-- passivize3 :: ThreePlacePred -> OnePlacePred
-- passivize3 r	= \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
