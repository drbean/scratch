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
	( "noe",	N ),
	( "alex",	A ),
	( "maria",	M ),
	( "money",	D ),
	( "stand",	S ),
	( "shoes",	O ),
	( "drugs",	H ),
	( "story",	Y ),
	( "teacher",	T ),
	( "school",	L ),
	( "construction_site",	C )


	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [N,A,T]
female	= pred1 [M]

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
shoes	= pred1 [O]
drugs	= pred1 [H]
stand	= pred1 [S]
story	= pred1 [Y]
construction_site	= pred1 [C]
school	= pred1 [L]

child	= pred1 [N]
teacher	= pred1 [A]

cried	= pred1 [M]

pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )

--(parent,child)
parenting	= [ (M,N) ]
--(husband,wife)
-- marriages	= [ (D,P) ]
--(initiator,wrongdoer?)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (N,M),(M,N),(N,D),(N,S),(A,D) ]
--(worker,site)
work	= [ (N,S),(N,C),(M,Unspec),(A,L) ]
teaching	= [ (A,N) ]
			
raised_by	= pred2 $ map swap parenting
have	= pred2 $ possessions ++ parenting
		++ ( map swap $ parenting )
worked_at	= pred2 work

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
worker	= pred1 $ map fst work

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)

met :: ThreePlacePred

meetings	= [ (N,A,L) ]
telling	= [ (N,S,A) ]
giving	= [ (N,D,M) ]

met	= pred3 meetings
gave	= pred3 giving
told	= pred3 telling

agent, theme, recipient, location :: (Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
location = recipient

recite = pred2 $ map ( \x -> (agent x, theme x) ) telling

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

int "money" = \ [x] -> money x
int "stand" = \ [x] -> stand x
int "teacher" = \ [x] -> teacher x
int "shoes" = \ [x] -> shoes x
int "drugs" = \ [x] -> drugs x
int "story" = \ [x] -> story x
int "construction_site" = \ [x] -> construction_site x
int "school" = \ [x] -> school x

int "worked" = \ args -> case args of [x] -> worker x; [x,y] -> worked_at y x
int "work" = int "worked"

int "met"	= \ [x,y,z] ->	met z y x;	int "meet"	= int "met"
int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "told" = \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x
int "tell" = int "told"


