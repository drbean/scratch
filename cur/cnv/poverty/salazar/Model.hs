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
	( "ken",	K ),
	( "john",	J ),
	( "leroy",	L ),
	( "henry",	H ),
	( "emma",	E ),
	( "money",	D ),
	( "farm",	F ),
	( "shoe",	O ),
	( "toy",	T ),
	( "bike",	B ),
	( "story",	Y ),
	( "work",	W ),
	( "law",	A ),
	( "school",	S ),
	( "colorado_college",	C ),
	( "michigan_law",	M )


	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [K,J,L,H]
female	= pred1 [E]

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

money	= pred1 [D]
shoe	= pred1 [O]
toy	= pred1 [T]
bike	= pred1 [B]
farm	= pred1 [F]
story	= pred1 [Y]
law	= pred1 [A]
school	= pred1 [C,M,S]
college	= pred1 [C]
law_school = pred1 [M]

child	= pred1 [K,J,L]


pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [  (H,L),(H,J),(H,K),(E,L),(E,J),(E,K) ]
--(husband,wife)
marriages	= [ (H,E) ]
--(divorcer,divorced)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (H,F),(L,O),(J,O),(K,O) ]

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(agent x,W) ) working )

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
worker	= pred1 $ map agent working

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)

met :: ThreePlacePred

meetings	= []
telling	= [ (K,Y,J),(J,Y,K) ]
giving	= [ (L,O,J) ]
--(worker,job,site)
working	= [ (H,Unspec,F) ]

met	= pred3 meetings
gave	= pred3 giving
told	= pred3 telling

agent, theme, recipient, location :: (Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
location = recipient

recite = pred2 $ map ( \x -> (agent x, theme x) ) telling
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working

--(teacher,school,subject,student)
schooling = [ (Unspec,C,Unspec,K),(Unspec,M,A,K), (Unspec,Unspec,Unspec,L),
			(Unspec,Unspec,Unspec,J) ]

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

--evaluation in model

type Interp a	= String -> [a] -> Bool

int :: Interp Entity
int "man"	= \ [x] -> man x
int "men"	= \ [x] -> man x
int "boy"	= \ [x] -> boy x; int "boys" = int "boy"
int "woman"	= \ [x] -> woman x
int "women"	= \ [x] -> woman x
int "girl"	= \ [x] -> girl x; int "girls" = int "girl"

int "person"	= \ [x] -> people x
int "thing"	= \ [x]	-> things x

int "parent" = \args -> case args of 
	[x] -> parent x
	[x,y] -> parented y x
int "parented" = \[x,y] -> parented y x
int "parents" = \ [x] -> parent x
int "mother" = \ [x] -> mother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "son" = \ [x] -> son x

int "money" = \ [x] -> money x
int "toys" = \ [x] -> toy x; int "toy" = int "toys"
int "shoes" = \ [x] -> shoe x; int "shoe" = int "shoes"
int "bikes" = \ [x] -> bike x; int "bike" = int "bikes"
int "story" = \ [x] -> story x
int "farm" = \ [x] -> farm x

int "work" = \args -> case args of
	[x] -> worker x
	[x,y] -> work_where y x || work_as y x
int "worked" = int "work"
int "had" = \[x,y] -> have y x;	int "have" = int "had"

int "met"	= \ [x,y,z] ->	met z y x;	int "meet"	= int "met"
int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "told" = \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x
int "tell" = int "told"
int "studied" = \args -> case args of
	[x,y] -> (studied_where y x || studied_what y x)
	[x,y,z] -> studied z y x
int "study" = int "studied"
int "went" = int "studied"
int "go" = int "went"

