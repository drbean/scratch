module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	= A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 


--alice, rex, kelley, judy, rock
--                                                :: Entity

characters :: [ (String, Entity) ]

characters = [
	( "mary",	M ),
	( "charles",	C ),
	( "william",	A ),
	( "eva",	E ),
	( "mrs_blaisdell",	B ),
	( "money",	D ),
	( "shoe",	O ),
	( "clothes",	L ),
	( "story",	Y ),
	( "work",	W ),
	( "wire",	I ),
	( "electrocution",	U ),
	( "house",	H ),
	( "butcher",	R ),
	( "bones",	N ),
	( "help",	P )

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [C,A]
female	= pred1 [M,E,B]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

people, things :: OnePlacePred

people	= \ x -> (male x || female x || x == Unspec)
things	= \ x -> (x == Unspec || not ( people x ) )

money	= pred1 [D]
shoe	= pred1 [O]
clothes	= pred1 [L]
help	= pred1 [P,M,E]
house	= pred1 [H]
wire	= pred1 [I]
story	= pred1 [Y]
butcher	= pred1 [R]
bones	= pred1 [N]

child	= pred1 [M]


pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [  (A,M),(E,M),(M,C) ]
--(husband,wife)
marriages	= [ (A,E) ]
--(divorcer,divorced)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (E,O),(E,L),(M,O),(M,L),(B,D) ]
laundering	= [ (M,L),(E,L) ]
cleaning	= [ (M,H),(E,H) ]
dropping	= [ (Unspec,I) ]
raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
married		= pred2 $ marriages ++ map swap marriages
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(agent x,W) ) working )
wash	= pred2 laundering
clean	= pred2 cleaning
drop	= pred2 dropping

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

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)

agent, theme, recipient, location, instrument ::
	(Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
location = recipient
instrument = recipient

meetings	= []
telling	= [ (M,Y,C) ]
giving	= [ (Unspec,O,E),(R,N,E),(R,N,M) ]
--(worker,job,site)
working	= [ (E,P,Unspec),(M,P,B) ]
--(killer,killed,instrument)
deaths	= [ (Unspec,A,U) ]

met	= pred3 meetings
gave	= pred3 giving
told	= pred3 telling
kill_with = pred3 deaths
kill = forgetful kill_with

die = passivize kill

worker	= pred1 $ map agent working
recite = pred2 $ map ( \x -> (agent x, theme x) ) telling
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
work_for	= pred2 $ map (\x -> (agent x, recipient x) ) working

--(teacher,school,subject,student)
schooling = []

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling

forgetful :: ThreePlacePred -> TwoPlacePred
forgetful r u v = or ( map ( r u v ) entities )
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
int "married"	= \ [x,y] -> married y x;	int "marry"	= int "married"
int "parents" = \ [x] -> parent x
int "mother" = \ [x] -> mother x
int "father" = \ [x] -> father x
int "daughter" = \ [x] -> daughter x
int "son" = \ [x] -> son x
int "butcher" = \ [x] -> butcher x

int "money" = \ [x] -> money x
int "shoes" = \ [x] -> shoe x; int "shoe" = int "shoes"
int "clothes" = \ [x] -> clothes x
int "story" = \ [x] -> story x
int "bones" = \ [x] -> bones x

int "washed" = \[x,y] -> wash y x; int "wash" = int "washed"
int "work" = \args -> case args of
	[x] -> worker x
	[x,y] -> work_where y x || work_as y x || work_for y x
int "worked" = int "work"
int "had" = \[x,y] -> have y x;	int "have" = int "had"
int "kill" = \args -> case args of [x,y] -> kill y x; [x,y,z] -> kill_with z y x
int "killed" = int "kill"
int "die"	= \[x] -> die x; int "died"	= int "die"

int "met"	= \ [x,y,z] ->	met z y x;	int "meet"	= int "met"
int "gave"	= \ [x,y,z] ->	gave z y x;	int "give"	= int "gave"
int "got" = \[x,y,z] -> gave x y z; int "get" = int "got"
int "told" = \ args -> case args of [x,y] -> recite y x; [x,y,z] -> told z y x
int "tell" = int "told"
int "studied" = \args -> case args of
	[x,y] -> (studied_where y x || studied_what y x)
	[x,y,z] -> studied z y x
int "study" = int "studied"
int "went" = int "studied"
int "go" = int "went"

