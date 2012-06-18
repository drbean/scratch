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

	( "connie",	C ),
	( "florida",	F ),
	-- ( "christopher",	H ),
	( "pensacola",	P ),
	( "robert",	R ),
	( "tom",	T )

	]

accident	= pred1 [A]
disability	= pred1 [Z]
body	= pred1 [B]
device	= pred1 [D]
beach	= pred1 [E]
horse	= pred1 [G]
-- friends	= pred1 [I]
lack_of_depression	= pred1 [K]
state	= pred1 [L]
hometown	= pred1 [M]
neck	= pred1 [N]
water_sports	= pred1 [O]
surfing	= pred1 [S]
part	= pred1 [X]
ventilator	= pred1 [V]
bowel_movement	= pred1 [W]

story	= pred1 [Y]

namelist = map fst characters

surfing_accident = accident

broken	= pred1 [N]
paralyzed	= pred1 [C,T]
resuscitated	= pred1 [C,T]
normal	= pred1 [R,C]

brain_damage	= pred1 [B]
brain_damaged	= pred1 []

appreciation = lack_of_depression
appreciative = appreciation

physically_disabled = pred1 [T]
mentally_disabled = pred1 []

male, female :: OnePlacePred

child	= pred1 []
male	= pred1 [T,R,H]
female	= pred1 [C]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

disabled, people, things :: OnePlacePred

people	= \ x -> (male x || female x || x == Unspec)
things	= \ x -> (x == Unspec || not ( people x ) )

boy	= \x -> male x && child x
isMan	= \x -> ( not $ boy x ) && male x
isGirl	= \x -> ( female x && child x )
isWoman	= \x -> ( not $ isGirl x ) && female x
isParent	= pred1 $ map fst parenting
isOffspring	= pred1 $ map snd parenting
isMother	= \x -> ( female x && isParent x )
isFather	= \x -> ( male x && isParent x )
daughter	= \x -> ( female x && isOffspring x )
son	= \x -> ( male x && isOffspring x )

disabled = \x -> ( mentally_disabled x || physically_disabled x )

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [(C,T),(R,T)]
help	= [(C,T),(R,T)]
feelings	= [(T,L)]
usage	= [(T,V)]
likes	= [(T,O),(R,O)]

possessions	= []
raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting

look_after	= parented
look_after_after = pred3 $ map (\(x,y) -> (x,y,A) ) parenting
take_care_of	= look_after
take_care_of_after = look_after_after
care_for	= look_after
care_for_after	= look_after_after

parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

have	= pred2 $ possessions ++ parenting
		++ ( map swap parenting )
		++ ( map (\x->(x, A) ) ( filter disabled entities ) )
		++ ( map (\x->(x, N) ) ( filter disabled entities ) )
		++ ( map (\x->(x, B) ) ( filter disabled entities ) )
used	= pred2 usage
felt	= pred2 feelings
appreciate	= pred2 $ map (\x -> (agent x, recipient x) ) $
			filter (\x-> appreciative $ theme x ) communications
thank	= appreciate

helped	= pred2 help
relied_on = pred2 $ map swap help

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
working	= [(R,Unspec,Unspec)]
--(agent,theme,recipient)
communications	= [(T,Y,C),(T,Y,R),(T,K,C),(T,K,R),(C,Unspec,T),(R,Unspec,T)]
--(agent,origin,destination)
movement	= [ (T,P,P) ]
--(source,acquisition,acquirer)
giving	= [(Unspec,C,R)]
acceptances = []
-- (seller, item, buyer)
selling	= []
--(killer,killed,instrument)
--(putter,theme,location)
--(agent,theme,location)
looking_back	= [(T,A,C)]
annoyances	= [(W,Unspec,R),(R,V,W),(R,V,K)]

annoyed	= pred2 ( map (\x -> (agent x, recipient x) ) annoyances )
offended	= annoyed
angered = offended
embarrassed = offended
look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back

worker	= pred1 $ map agent working
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working

said	= pred2 $ map (\x->(agent x, theme x) ) communications
asked	= pred2 $ map (\x->(agent x, recipient x) ) communications
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) communications
talked	= pred2 $ map (\x->(agent x, recipient x) ) communications ++
	map (\(agent,theme,recipient)->(recipient, agent) ) communications
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) communications


gave	= pred3 giving
got_from    = pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
got = forgetful got_from
sold	= pred2 $ map (\x -> (agent x, theme x) ) selling

come_from	= pred2 $ map (\x->(agent x, origin x) ) movement
left	= come_from
grew_up_in	= come_from
go_to	= pred2 $ map (\x->(agent x, destination x) ) movement
move	= pred3 movement

told	= pred3 communications

recite = pred2 $ map ( \x -> (agent x, theme x) ) communications

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
period4 = theme4
recipient4 (_,_,_,r) = r

-- (agent,location,period,recipient)
cohabitation	= [(T,P,Unspec,R),(T,P,Unspec,C),(T,F,Unspec,R),(T,F,Unspec,C)] 
lived_with_in_for	= pred4 $ map ( \x ->
	(agent4 x, recipient4 x, location4 x, period4 x) ) cohabitation
lived_with_in	= pred3 $ map ( \x -> (agent4 x, recipient4 x, location4 x) )
					cohabitation
lived_with_for	= pred3 $ map ( \x -> (agent4 x, recipient4 x, period4 x) )
					cohabitation
lived_with	= forgetful lived_with_in
lived_in	= pred2 $ map (\x -> (agent4 x, location4 x) ) cohabitation
			++ map (\x -> (recipient4 x, location4 x) ) cohabitation

-- (teacher,school(location),subject,student)
schooling = []
studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling
teach = pred3 $ map (\x -> (agent4 x, theme4 x, recipient4 x) ) schooling
teach_what = forgetful teach
teach_who = pred2 $ map (\x -> (agent4 x, recipient4 x) ) schooling
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
