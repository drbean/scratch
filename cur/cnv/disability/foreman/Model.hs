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
	( "south_africa",	A ),
	( "amanda",	W ),
	( "rita",	K ),
	( "new_york",	N ),
	( "california",	F ),
	( "michelle",	M ),
	( "mia",	I ),
	( "renee",	R )
	]


operation	= pred1 [O]
electrolarynx	= pred1 [X]
voice	= pred1 [V]
gym	= pred1 [G]
restaurant	= pred1 [U]
throat_cancer	= pred1 [C]
disability	= pred1 [L]
sound	= pred1 [D]
hands_on_hips	= pred1 [H]
phone	= pred1 [P]
computer	= pred1 [E]
joke	= pred1 [J]
telemarketers	= pred1 [T]
story	= pred1 [Y]
year	= pred1 [B]

namelist = map fst characters

funny	= sound

shrinking_violet	= pred1 [I]
kind	= pred1 [K]
scared	= pred1 [R]
older	 = scared
physically_disabled = scared
movie_star = scared

mentally_disabled = pred1 []

male, female :: OnePlacePred

child	= pred1 []
male	= pred1 []
female	= pred1 [M,I,R,W,K]

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
parenting	= [(R,I),(R,M)]
help	= [(M,R)]
possessions	= [(R,X)]
appreciation	= [(R,K)]
illness	= [(R,C)]
greeting	= [(K,R)]
feelings	= [(R,R)]
resemblances	= [(R,E)]
raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting

parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

have	= pred2 $ possessions ++ parenting ++ illness
		++ ( map swap parenting )
		++ ( map (\x->(fst x, O) ) illness )
		++ ( map (\x->(x, L) ) ( filter disabled entities ) )
felt	= pred2 feelings
sounded	= pred2 resemblances
appreciate	= pred2 appreciation
thank	= appreciate

-- greet	= interview
helped	= pred2 $ help

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
working	= [(K,Unspec,U)]
communications	= [ (R,Y,M),(M,Y,R),(R,D,W),(R,Unspec,K),(R,Unspec,T),
			(T,Unspec,R) ]
--(agent,origin,destination)
movement	= [ (R,A,F),(M,N,F) ]
--(source,acquisition,acquirer)
giving	= [(Unspec,C,R)]
acceptances = []
-- (seller, item, buyer)
selling	= []
--(killer,killed,instrument)
--(putter,theme,location)
--(agent,theme,location)
looking_back	= [(R,Y,Unspec),(M,Y,Unspec)]
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
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
got	= forgetful got_from
sold	= pred2 $ map (\x -> (agent x, theme x) ) selling

come_from	= pred2 $ map (\x->(agent x, origin x) ) movement
left	= come_from
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
cohabitation	= [(M,F,B,R),(R,F,B,M),(M,N,Unspec,Unspec)] 
lived_with_in_for	= pred4 $ map ( \x ->
	(agent4 x, recipient4 x, location4 x, period4 x) ) cohabitation
lived_with_in	= pred3 $ map ( \x -> (agent4 x, recipient4 x, location4 x) )
					cohabitation
lived_with_for	= pred3 $ map ( \x -> (agent4 x, recipient4 x, period4 x) )
					cohabitation
lived_with	= forgetful lived_with_in
lived_in	= pred2 $ map (\x -> (agent4 x, location4 x) ) cohabitation	

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
