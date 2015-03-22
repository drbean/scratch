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
	( "steve_wynn",	W )
	, ( "the_ferrari_showroom",	R )
	-- ( "pepsi",	P ),
	-- ( "punjabi_farmers",	AF ),
	-- ( "the_punjabi_government",	AP ),
	-- ( "alex_tew",	A ),
	-- ( "the_million_dollar_homepage",	H ),
	-- ( "facebook",	F ),
	-- ( "mark_zuckerberg",	M )

	]

male, female :: OnePlacePred


female	= pred1 []
male	= pred1 [W,B,AV,AB]
role	= pred1 []
child	= pred1 [AC]

showroom	= pred1 [R]
crowded	= pred1 [R]
visitors	= pred1 [V,B,AV,AB]
buyer	= pred1 [B]
cars	= pred1 [C]
expensive	= pred1 [C]
ten_dollars	= pred1 [T]
entrance_fee	= pred1 [T]

oranges	= pred1 [O]

website	= pred1 [M,F]
founder	= pred1 [M,F]

good	= pred1 [AG]
bad	= pred1 [AB]

experiment	= pred1 [AE]
successful	= pred1 [R,AE]
unsuccessful	= pred1 []


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

acquainted	= [(V,R),(B,R),(AV,R),(AB,R),(W,R)]
know	= pred2 $ acquainted ++ map (\x -> (snd x, fst x)) acquainted
have	= pred2 $ possessions ++ management ++ family ++ patronage
		-- ++ ( map (\x->(x,N) ) $ filter person entities ) -- personality
family	= [(AV,AC),(AC,AV)]
patronage = [(R,V),(R,B),(R,AV),(R,AC),(R,AB)]
finish	= pred3 []
pay	= pred3 [(W,T,Unspec),(V,T,Unspec),(AV,T,Unspec),(AB,T,Unspec)]
enter	= pred2 [(W,R),(V,R),(B,R),(AV,R),(AC,R),(AB,R)]

pay_to_enter :: ThreePlacePred
pay_to_enter	= pred3 [(V,V,R),(AV,AV,R),(AB,AB,R)]

-- offered_to_pay	= pred3 [(P,P,AF)]

buy = pred2 [(B,C)]

decided_to_charge :: ThreePlacePred
decided_to_charge	= pred3 [(W,W,T) ]
decided_to_charge_fee	= pred4 [(W,W,T,V) ]
decided_to_charge_entry	= pred5 [(W,W,T,V,R) ]

wanted_to_look :: TwoPlacePred
wanted_to_look	= pred2 [(P,P) ]
wanted_to_look_at	= pred3 [(V,V,C) ]

wanted_to_buy :: TwoPlacePred
wanted_to_buy	= pred2 []
wanted_to_buy_some	= pred3 [(B,B,C)]

wanted_to_pay :: ThreePlacePred
wanted_to_pay	= pred3 []

have_to_pay :: ThreePlacePred
have_to_pay = pred3 [(V,V,T),(AV,AV,T),(AB,AB,T)]

-- offered_to_buy :: ThreePlacePred
-- offered_to_buy	= pred3 [(P,P,O)]
-- offered_to_buy_from :: FourPlacePred
-- offered_to_buy_from	= pred4 [(P,P,O,AF)]

possessions	= [(W,C),(W,R),(R,C)]
management	= [(W,R)]

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
instrument = recipient
origin	= theme
destination = recipient

gave	= pred3 giving
got	= pred2 $ map (\x -> (recipient x, patient x) ) giving
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

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

giving	= []
comms	= [ (M,N,L),(L,N,M),(M,S,L),(L,S,M) ]
work	= [ (B,G,M),(Unspec,W,L)]

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

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
