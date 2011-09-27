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
	( "pj",	P ),
	( "sam",	S ),
	( "dan",	D ),
	( "dick",	K ),
	( "bar",	B ),
	( "fake_id",	I ),
	( "phone_number",	N ),
	( "story",	Y ),
	( "teacher",	T ),
	( "ann",	A )


	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [S,D,K]
female	= pred1 [P,A]

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

bar	= pred1 [B]
fake_id	= pred1 [I]
teacher	= pred1 [T]
child	= pred1 [S]
phone_number = pred1 [N]
story	= pred1 [Y]

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

--(parent,child)
parenting	= [ (D,S),(P,S),(A,P) ]
--(husband,wife)
marriages	= [ (D,P) ]
--(initiator,wrongdoer?)
separations	= [ (D,P) ]
divorces	= []
--(boyfriend,girlfriend)
unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (P,I) ]
yelling		= [ (P,D) ]
friendships	= [ (D,K) ]
--(worker,job)
work	= [ (P,T),(D,Unspec),(K,Unspec) ]
			
parented, married ::  TwoPlacePred

parented	= pred2 parenting
raised_by	= pred2 $ map swap parenting
married		= pred2 $ marriages ++ map swap marriages
divorced	= pred2 divorces
separated	= pred2 separations
have	= pred2 $ possessions ++ parenting ++ marriages ++ unmarried_couples
		++ ( map swap $ parenting ++ marriages ++ unmarried_couples )
knew	= \x y -> ( parented x y || raised_by x y || married x y || divorced x y
		)
yelled_at	= pred2 $ yelling ++ map swap yelling
worked_as	= pred2 work

boy	= \x -> male x && child x
man	= \x -> ( not $ boy x ) && male x
girl	= \x -> ( female x && child x )
woman	= \x -> ( not $ girl x ) && female x
parent	= pred1 $ map fst parenting
offspring	= pred1 $ map snd parenting
mother	= \x -> ( female x && parent x )
father	= \x -> ( male x && parent x )
daughter	= \x -> ( female x && offspring x )
son	= \x -> ( male x && offspring x )
boyfriend	= pred1 $ map fst unmarried_couples
girlfriend	= pred1 $ map snd unmarried_couples
worker	= pred1 $ map fst work

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

met :: ThreePlacePred

meetings	= [ (D,P,B) ]
telling	= [ (P,S,Y) ]
giving	= [ (P,N,D) ]

met	= pred3 meetings
gave	= pred3 giving
told	= pred3 telling

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

-- passivize3 :: ThreePlacePred -> OnePlacePred
-- passivize3 r	= \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

--evaluation in model

type Interp a	= String -> [a] -> Bool

int :: Interp Entity
int "man"	= \ [x] -> man x
int "men"	= \ [x] -> man x
int "woman"	= \ [x] -> woman x
int "women"	= \ [x] -> woman x

int "person"	= \ [x] -> people x
int "thing"	= \ [x]	-> things x

int "parent" = \ [x] -> parent x
int "parents" = \ [x] -> parent x
int "mother" = \ [x] -> mother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "boyfriend" = \[x] -> boyfriend x
int "girlfriend" = \[x] -> girlfriend x

int "phone_number" = \ [x] -> phone_number x
int "teacher" = \ [x] -> teacher x
int "fake_id" = \ [x] -> fake_id x
int "story" = \ [x] -> story x
int "bar" = \ [x] -> bar x

int "worked" = \ args -> case args of [x] -> worker x; [x,y] -> worked_as y x
int "work" = int "worked"

int "parented"	= \ [x,y] -> parented y x
int "married"	= \ [x,y] -> married y x;	int "marry"	= int "married"
int "divorced"	= \ [x,y] -> divorced y x;	int "divorce"	= int "divorced"
int "separated"	= \ [x,y] -> separated y x;	int "separate" = int "separated"
int "had"	= \ [x,y] -> have y x
int "have"	= \ [x,y] -> have y x

int "met"	= \ [x,y,z] ->	met z y x;	int "meet"	= int "met"
int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "told"	= \ [x,y,z] ->	told z y x;	int "tell"	= int "told"


