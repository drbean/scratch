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
	( "theresa",	T ),
	( "stephanie",	S ),
	( "mai",	F ),
	( "matthew",	B ),
	( "culture",	C ),
	( "upbringing",	G ),
	( "boy",	O ),
	( "necklace",	N ),
	( "vietnam",	V ),
	( "vietnamese",	M ),
	( "english",	E ),
	( "the_united_states",	U ),
	( "present",	P ),
	( "sense",	I ),
	( "right",	R ),
	( "wrong",	W ),
	( "disappointment",	K ),
	( "story",	Y)

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [F,B,O]
female	= pred1 [T,S]

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

culture	= pred1 [C]
upbringing	= pred1 [G]
necklace	= pred1 [N]
present	= pred1 [P]
sense	= pred1 [I]
language	= pred1 [E,V]
disappointment	= pred1 [K]
story	= pred1 [Y]

child	= pred1 [S,B]

boy	= \x -> male x && child x
isMan	= \x -> ( not $ boy x ) && male x
isGirl	= \x -> ( female x && child x )
isWoman	= \x -> ( not $ isGirl x ) && female x
isParent	= pred1 $ map fst parenting
isOffspring	= pred1 $ map snd parenting
isMother	= \x -> ( female x && isParent x )
father	= \x -> ( male x && isParent x )
daughter	= \x -> ( female x && isOffspring x )
son	= \x -> ( male x && isOffspring x )

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [  (T,S),(F,S),(T,B),(F,B) ]
--(husband,wife)
marriages	= [ (F,T) ]
--(divorcer,divorced)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (Unspec,D),(M,O),(M,L),(B,D) ]

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
married		= pred2 $ marriages ++ map swap marriages
parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

disappointment = []
disappoint	= pred2 $ disappointment
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(recipient x, theme x) ) giving )
knowledge	= [(F,E),(F,V),(T,E),(T,V),(S,E),(S,V),(B,E),(B,V)]
acquaintances	= [ (O,S) ]
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
speak	= \x y -> language y && know x y

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

comms	= [ (T,G,S) ]
giving	= [ (O,N,S) ]
--(killer,killed,instrument)
--(putter,theme,location)
cooking = []
--(agent,theme,location)
looking_back	= [(T,G,Unspec)]
seeing	= []

look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back
said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms


gave	= pred3 giving
told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

-- (teacher,school,subject,student)
schooling = []
studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling
student = pred1 $ map recipient4 schooling

forgetful :: ThreePlacePred -> TwoPlacePred
forgetful r u v = or ( map ( r u v ) entities )
passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
