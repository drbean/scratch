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
	( "troop_409",	T ),
	( "derek",	D ),
	( "richard",	R ),
	( "claudia",	C ),
	( "randy",	A )
	]


mentally_disabled	= pred1 [D]
physically_disabled	= pred1 [A]
scout	= pred1 [D,A]
scoutmaster	= pred1 [R]
older	= pred1 [D,R,C]
assistant_scoutmaster	= pred1 [C]
eagle_scout	= pred1 [D,R]
leader	= pred1 [R,C]
troop	= pred1 [T]
story	= pred1 [Y]
job	= pred1 []
traffic_accident	= pred1 [F]
brain_damage	= pred1 [G]

namelist = map fst characters

dedicated	= mentally_disabled

male, female :: OnePlacePred

child	= pred1 [A]
male	= pred1 [A,D,R]
female	= pred1 [C,X,Z]

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

disabled = \x -> ( mentally_disabled x || physically_disabled x )

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [(R,X),(R,Z),(C,X),(C,Z)]
supervision	= [(R,A),(R,D),(C,A),(C,D)]
help	= [(D,A),(D,R),(D,C),(C,R)]
marriages	= [(R,C)]
--(husband,wife,wedding_location)
weddings	= [(R,C,Unspec)]
--(divorcer,divorced)
separations	= []
divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [] ++ []
recruitment	= [(Unspec,D,T),(R,A,T),(R,C,T),(Unspec,R,T)]
appreciation	= [(C,D),(R,D)]
accidents	= [(A,F),(D,G)]

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
marry_in	= pred3 $ weddings ++ map (\(x,y,z) -> (y,x,z) ) weddings
married		= forgetful marry_in
husband	= pred1 $ map agent weddings
wife	= pred1 $ map theme weddings
separated	= pred2 separations
wedded_in	= pred2 $ map (\x -> (agent x, location x) ) weddings ++
			map (\x -> (patient x, location x) ) weddings
isMarried	= pred1 $ map fst marriages ++ map snd marriages
parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

supervisor	= pred1 $ map fst supervision
subordinate	= pred1 $ map snd supervision

clothing	= []
losses	= []
looking	= []
wore	= pred2 clothing
have	= pred2 $ possessions ++ marriages ++ parenting ++ supervision
		++ accidents
		++ ( map swap $ marriages ++ parenting ++ supervision )
		++ ( map (\x->(recipient x, theme x) ) giving )
		++ ( map (\x->(agent x,J) ) working )
		++ ( map (\x->(agent x, patient x) ) recruitment )
		++ ( map (\x->(agent x, location x) ) recruitment )
knowledge	= []
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
appreciate	= pred2 appreciation
thank	= appreciate
membership	= pred2 $ map (\x -> (patient x, location x) ) recruitment

-- visit	= pred2 $ map (\x -> (patient x, recipient x) ) recruitment
interview	= pred2 $ map (\x -> (agent x, patient x) ) recruitment
-- greet	= interview
look_at	= pred2 $ looking
lose	= pred2 $ losses
helped	= pred2 $ supervision ++ help

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
working	= []
volunteering = [(R,S,T),(C,I,T)]
communications	= [ (R,Y,D),(C,Y,D) ]
offenses	= []
giving	= []
acceptances = []
-- (seller, item, buyer)
selling	= []
--(killer,killed,instrument)
--(putter,theme,location)
--(agent,theme,location)
looking_back	= [(R,Y,Unspec),(D,Y,Unspec),(C,Y,Unspec)]

worker	= pred1 $ map agent working
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
volunteer_at	= pred2 $ map (\x -> (agent x, location x) ) volunteering
look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back
said	= pred2 $ map (\x->(agent x, theme x) ) communications
asked	= pred2 $ map (\x->(agent x, recipient x) ) communications
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) communications
talked	= pred2 $ map (\x->(agent x, recipient x) ) communications ++
	map (\(agent,theme,recipient)->(recipient, agent) ) communications
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) communications
offend_with	= pred3 offenses
offend	= pred2 $ ( map (\x -> (agent x, recipient x) ) offenses ) ++
		( map (\x -> (theme x, recipient x) ) offenses )
anger = offend


gave	= pred3 giving
got	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
sold	= pred2 $ map (\x -> (agent x, theme x) ) selling

told	= pred3 communications

recite = pred2 $ map ( \x -> (agent x, theme x) ) communications

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

-- (teacher,school(location),subject,student)
schooling = [(Unspec,Unspec,V,D)]
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
