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
	( "gus",	G ),
	( "ileana",	I ),
	( "fidel",	F ),
	( "ofelia",	O ),
	( "cuba",	C ),
	( "spanish",	H ),
	( "english",	E ),
	( "the_united_states",	U ),
	( "medical_school",	S ),
	( "medicine",	N ),
	( "doctor",	R ),
	( "tomatoes",	T ),
	( "fields",	J ),
	( "cleaning",	P ),
	( "motel",	L ),
	( "money",	M ),
	( "doll",	D ),
	( "boat",	B ),
	( "disappointment",	K ),
	( "upbringing",	V ),
	( "story",	Y)

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [G,F]
female	= pred1 [I,O]

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

culture = pred1 []
necklace	= pred1 []
sense	= pred1 []
present	= pred1 [D]
doll	= pred1 [D]
boat	= pred1 [B]
language	= pred1 [E,H]
medical_school	= pred1 [S]
medicine	= pred1 [N]
doctor	= pred1 [R]
tomato	= pred1 [T]
fields	= pred1 [J]
cleaning	= pred1 [L]
motel	= pred1 [L]
disappointment	= pred1 [K]
money	= pred1 [M]
upbringing	= pred1 [V]
story	= pred1 [Y]

child	= pred1 [I]

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
parenting	= [  (G,I),(O,I) ]
--(husband,wife,wedding_location)
marriages	= [ (G,O) ]
weddings	= [ (G,O,C) ]
--(divorcer,divorced)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (I,D),(Unspec,B) ]
appreciation	= [ (I,G) ]

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
marry_in	= pred3 $ weddings ++ map (\(x,y,z) -> (y,x,z) ) weddings
married		= forgetful marry_in
wedded_in	= pred2 $ map (\x -> (agent x, location x) ) weddings ++
			map (\x -> (patient x, location x) ) weddings
isMarried	= pred1 $ map fst marriages ++ map snd marriages
parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

disappointments = [(F,G)]
disappoint	= pred2 $ disappointments
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(recipient x, theme x) ) giving )
knowledge	= [(G,E),(G,H),(O,E),(O,H),(I,E),(I,H),(G,U),(G,C),(O,U),(O,C),(I,U),(I,C),(G,N)]
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
speak	= \x y -> language y && know x y
appreciate	= pred2 appreciation

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)

agent, theme, recipient, location, instrument ::
	(Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
patient = theme
location = recipient
instrument = recipient
origin	= theme
destination = recipient

--(worker,job,site)
working	= [ (G,R,C),(G,R,U),(G,T,J),(G,P,L) ]
comms	= [ (G,Y,I) ]
giving	= [ (G,D,I) ]
-- (seller, item, buyer)
selling	= [ (O,D,Unspec) ]
--(killer,killed,instrument)
--(putter,theme,location)
cooking = []
--(agent,theme,location)
looking_back	= [(G,C,V),(I,C,V)]
seeing	= []
--(agent,origin,destination)
immigration	= [ (G,C,U),(O,C,U),(I,C,U),(Unspec,C,U) ]

isImmigrant	= pred1 $ map agent immigration
worker	= pred1 $ map agent working
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back
said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
come_from	= pred2 $ map (\x->(agent x, origin x) ) immigration
go_to	= pred2 $ map (\x->(agent x, destination x) ) immigration ++
			map (\x->(recipient4 x,location4 x) ) schooling
immigrate	= pred3 immigration


gave	= pred3 giving
got	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
sold	= pred2 $ map (\x -> (agent x, theme x) ) selling

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

-- (teacher,school(location),subject,student)
schooling = [(Unspec,S,N,G),(Unspec,C,N,G),(Unspec,U,N,G)]
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
