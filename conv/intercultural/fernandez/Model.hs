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
	( "jose",	J ),
	( "teresita",	W ),
	( "jose's_father",	F ),
	( "jose's_brother",	B ),
	( "jack_johnson",	A ),
	( "thanksgiving",	T ),
	( "class",	C ),
	( "english",	E ),
	( "the_united_states",	U ),
	( "patron_saint",	S ),
	( "missal",	M ),
	( "spanish",	H ),
	( "dinner",	D ),
	( "pumpkin_pie",	P ),
	( "piece",	I ),
	( "oven",	O ),
	( "story",	Y ),
	( "knife",	K )

	]

names :: [( Entity, String )]

names = map swap characters

male, female :: OnePlacePred

male	= pred1 [J,F,B,A]
female	= pred1 [W]

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

thanksgiving	= pred1 [T]
klass	= pred1 [C]
english	= pred1 [E]
united_states	= pred1 [U]
patronsaint	= pred1 [S]
missal	= pred1 [M]
spanish	= pred1 [H]
language	= pred1 [E,H]
dinner	= pred1 [D]
pumpkin_pie	= pred1 [P]
piece	= pred1 [I]
oven	= pred1 [O]
knife	= pred1 [K]

child	= pred1 [J,B]

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
parenting	= [  (F,J),(F,B) ]
--(husband,wife)
marriages	= [ (J,W) ]
--(divorcer,divorced)
-- separations	= [ (D,P) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
eating	= [ (F,I),(J,I) ]
possessions	= [ (Unspec,D),(M,O),(M,L),(B,D) ]

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
married		= pred2 $ marriages ++ map swap marriages
parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> map (parentMaybe child) parenting
siblings	= \a b -> (any . flip elem) (parents a) (parents b)
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(recipient x, theme x) ) giving )
		++ eating
knowledge	= [ (J,Unspec),(B,Unspec),(A,E),(F,E),(W,E),(J,H),(B,H),(F,H),(W,H) ]
know	= pred2 knowledge
speak	= \x y -> language y && know x y
eat	= pred2 eating

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

communications	= [ (J,Unspec,A),(B,Unspec,A),(A,Unspec,F),(F,Unspec,J),(F,Unspec,B),(J,Y,W) ]
giving	= [ (Unspec,P,F),(Unspec,P,J) ]
--(killer,killed,instrument)
cuts	= [ (J,P,K),(F,P,K) ]
--(putter,theme,location)
cooking	= [ (Unspec,P,O),(A,P,O) ]
--(agent,theme,location)
looking_for	= [ (J,Unspec,M),(B,Unspec,M) ]
seeing	= []

said	= pred2 $ map (\x->(agent x, theme x) ) communications
asked	= pred2 $ map (\x->(agent x, recipient x) ) communications


gave	= pred3 giving
told	= pred3 communications
cut_with	= pred3 cuts
cut	= forgetful cut_with
bake	= pred3 cooking

recite = pred2 $ map ( \x -> (agent x, theme x) ) communications

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

-- (teacher,school,subject,student)
schooling = [(Unspec,Unspec,Unspec,J),(Unspec,Unspec,Unspec,B)]
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
