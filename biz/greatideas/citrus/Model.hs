module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	=
              A | B | C | D | E | F | G 
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z
            | AA | AB | AC | AD | AE | AF | AG 
            | AH | AI | AJ | AK | AL | AM | AN 
            | AO | AP | AQ | AR | AS | AT | AU 
            | AV | AW | AX | AY | AZ
	    | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 


characters :: [ (String, Entity) ]

characters = [
	( "pepsi",	P )
	, ( "punjab",	AP )
	, ( "punjabi_farmers",	AF )
	, ( "the_punjabi_government",	AG )

	]

male, female :: OnePlacePred


female	= pred1 []
male	= pred1 [AF]
role	= pred1 []
child	= pred1 []

oranges	= pred1 [O]
citrus_fruit	= pred1 [O]
land	= pred1 [L]

money	= pred1 [M]
good_idea	= pred1 [I]
a_good_price	= pred1 [AP]

good	= pred1 [AG]
bad	= pred1 [AB]

experiment	= pred1 [I]
successful	= pred1 [P]
unsuccessful	= pred1 [I]


namelist = map fst characters

names = map swap characters

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool
type FourPlacePred	= Entity -> Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

person, thing :: OnePlacePred

person	= \ x -> (male x || female x || role x || x == Unspec)
thing	= \ x -> (x == Unspec || not ( person x ) )

boy	= \x -> male x && child x
isMan	= \x -> ( not $ boy x ) && male x
isGirl	= \x -> ( female x && child x )
isWoman	= \x -> ( not $ isGirl x ) && female x
boss = pred1 $ map fst management

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )
pred5 xs	= curry5 ( `elem` xs )

acquainted	= [(P,O),(P,AG),(AF,AG),(P,AF)]
know	= pred2 $ acquainted ++ map swap acquainted
have	= pred2 $ possessions ++ management ++ family ++ patronage
		-- ++ ( map (\x->(x,N) ) $ filter person entities ) -- personality
family	= []
patronage = []
finish	= pred3 []
purchases	= [(P,O,AF,Unspec)]
buy	= pred3 $ map (\x -> (agent4 x, theme4 x, provider4 x) ) purchases
pay = pred4 $ map (\x -> (agent4 x, provider4 x, theme4 x, purpose4 x) ) purchases

wanted = buy
wanted_to_buy :: FourPlacePred
wanted_to_buy	= pred4 $
	map ( \x -> (agent4 x, agent4 x, theme4 x, provider4 x) ) purchases

sell :: ThreePlacePred
sell	= pred3 [(AF,O,P)]

wanted_to_sell :: FourPlacePred
wanted_to_sell	= pred4 [(AF,AF,O,P)]

grow :: TwoPlacePred
grow	= pred2 [(AF,O),(AG,O)]

wanted_to_grow :: ThreePlacePred
wanted_to_grow	= pred3 [(P,AF,O),(P,AG,O),(AF,AF,O),(AG,AG,O),(AG,AF,O)]

help :: TwoPlacePred
help	= pred2 [(AG,AF)]

wanted_to_help :: ThreePlacePred
wanted_to_help	= pred3 [(AG,AG,AF)]

make :: TwoPlacePred
make	= pred2 [(AF,M)]

helped_to_make :: ThreePlacePred
helped_to_make	= pred3 [(AG,AF,M)]

wanted_to_pay :: ThreePlacePred
wanted_to_pay	= pred3 []

have_to_pay :: ThreePlacePred
have_to_pay = pred3 []

offered_to_buy :: ThreePlacePred
offered_to_buy	= pred3 [(P,P,O)]
offered_to_buy_from :: FourPlacePred
offered_to_buy_from	= pred4 [(P,P,O,AF)]

possessions	= [(AF,L)]
management	= [(AG,L)]

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)
curry5 f x y z w u	= f (x,y,z,w,u)

agent, theme, recipient, location, instrument ::
	(Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
patient = theme
location = recipient
source	= recipient
instrument = recipient
origin	= theme
destination = recipient

gave	= pred3 giving
got	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talk_with_or_about	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(agent, theme) ) comms
talk_with_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
		++ comms
told	= pred3 comms
recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

worker	= pred1 $ map patient work
work_where	= pred2 $ map (\x -> (patient x, agent x) ) work
work_as = pred2 $ map (\x -> (patient x, location x) ) work

giving	= [(AF,L,AG)]
comms	= [ (P,O,AG),(AG,O,AF),(AG,L,AF) ]
work	= [ (AF,AF,L)]

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_)	= a
theme4 (_,t,_,_)	= t
recipient4 (_,_,r,_)	= r
provider4	= recipient4
location4 (_,_,_,l)	= l
purpose4	= location4

unditrans :: FourPlacePred -> ThreePlacePred
unditrans r u v w = or ( map ( r u v w ) entities )

forgetful :: ThreePlacePred -> TwoPlacePred
forgetful r u v = or ( map ( r u v ) entities )

intransit :: TwoPlacePred -> OnePlacePred
intransit r u = or ( map ( r u ) entities )

reflexivize f x y = f y x

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
