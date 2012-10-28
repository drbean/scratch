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
	( "one_month",	AO ),
	( "two_months",	AW ),
	( "6_000_dollars",	AS ),
	( "3_000_dollars",	AT ),
	( "50_dollars_an_hour",	AF ),
	( "larry",	L ),
	( "michelle",	M )

	]

male, female :: OnePlacePred


female	= pred1 [M]
male	= pred1 [L]
role	= pred1 [L,M]
child	= pred1 []

bookstore	= pred1 [F]
manager	= pred1 [M]
website_designer	= pred1 [L]
negotiation	= pred1 [N]
website	= pred1[S]

good	= pred1 [AG]
bad	= pred1 [AB]


namelist = map fst characters

names = map swap characters

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

people, things :: OnePlacePred

people	= \ x -> (male x || female x || role x || x == Unspec)
things	= \ x -> (x == Unspec || not ( people x ) )

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

have	= pred2 $ possessions ++ management
		-- ++ ( map (\x->(x,N) ) $ filter people entities ) -- personality

data Want a where
	Thing :: Entity -> Want Entity
	Promo :: Entity -> Want Entity
	Event :: (Entity,Entity,Entity) -> Want Entity
wanted_pay :: ThreePlacePred
wanted_pay	= pred3 [(M,M,L),(L,M,AF)]
wanted_pay_sum	= pred4 [(M,M,AS,L)]
wanted_finish	= pred4 [(L,L,S,AW),(M,L,S,AO)]
wanted_charge	= pred4 [(L,L,AF,M)]

possessions	= []
management	= [(M,B)]

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

gave	= pred3 giving
got	= pred2 $ map (\x -> (recipient x, patient x) ) giving
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
told	= pred3 comms
recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

worker	= pred1 $ map patient work
work_where	= pred2 $ map (\x -> (patient x, agent x) ) work
work_as = pred2 $ map (\x -> (patient x, location x) ) work

giving	= []
comms	= [ (M,N,L),(L,N,M) ]
work	= [ (B,G,M),(Unspec,W,L)]

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

forgetful :: ThreePlacePred -> TwoPlacePred
forgetful r u v = or ( map ( r u v ) entities )
passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 
