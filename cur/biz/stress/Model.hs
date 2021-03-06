module Model where 

import Data.Tuple

data Entity	= A | B | C | D | E | F | G 
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z 
            | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 

characters :: [ (String, Entity) ]

characters = [
	( "a-ho",	A ),
	( "ellen_macarthur",	E ),
	( "dr_bean",	T ),
	( "steve_fossett",	S ),
	( "european_campers",	M ),
	( "carrefour",	F ),
	( "charles_holden",	J ),
	( "jacques",	Q ),
	( "olivier",	V ),
	( "todd",	O ),
	( "alan",	L ),
	( "david",	I ),
	( "dot_gourlay",	G ),
	( "tan",	N ),
	( "cusp",	K ),
	( "slow_living",	W )

	]

classes :: [String]
classes = [ "manager", "advertisers", "advertising_space", "business_management", "radio_and_television" ]

lack_of_control	= [C]
uncertainty	= [U]
lack_of_support	= [H]
pressure	= pred1 [P]
stress	= pred1 [R]

adventurer	= pred1 [S,E]
teacher	= pred1 [T]
psychologist	= pred1 [A,D,T]
doctor	= pred1 [D]
office_worker	= pred1 [A,B,C,D]
ceo	= pred1 [J]
production_manager	= pred1 [Q]
salesman	= pred1 [V]
sales_manager	= pred1 [O]
customer	= pred1 [F]

context :: [Entity]
-- context = [A,H,P,F,M]
context = []

order	= pred1 [Unspec]
goods	= pred1 [Unspec]
company	= pred1 [M,F,Unspec]
framework	= pred1 [K]

namelist :: [String]
namelist = map fst characters
names :: [(Entity, String)]
names = map swap characters

angry	= pred1 [Q,V]
useful	= pred1 [K]

predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid4 :: String -> FourPlacePred
predid1 name = lookupPred name onePlacers where
	lookupPred name []	= error $ "no \"" ++ name ++ "\" one-place predicate."
	lookupPred name ((n,p):_) | n == name	= p
	lookupPred name (i:is) = lookupPred name is
predid2 name = lookupPred name twoPlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" two-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is
predid3 name = lookupPred name threePlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" three-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is
predid4 name = lookupPred name fourPlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" four-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is

large	= order
brilliant	= salesman
rude	= brilliant
best	= rude

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
	("true",	pred1 entities)
	, ("false",	pred1 [])
	, ("male",	pred1 [S,Q,V,C,O,L,I])
	, ("female",	pred1 [E,P,N])
	, ("manager",	pred1 [Q,O])
	, ("boss",	pred1 $ map fst management)
	, ("role",	pred1 [])
	, ("person",	person)
	, ("thing",	thing)

	, ("good",	pred1 [])
	, ("bad",	pred1 [C,U,H,P,R])

	, ("experiment",	pred1 [H,P,F])
	, ("successful",	pred1 [H,F])
	, ("unsuccessful",	pred1 [P])
	]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool
type FourPlacePred	= Entity -> Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

person, thing :: OnePlacePred
person	= \ x -> (predid1 "male" x || predid1 "female" x || predid1 "role" x || x == Someone)
thing	= \ x -> (x == Unspec || x == Something || not ( person x ) )

-- boy	= \x -> male x && child x
-- isMan	= \x -> ( not $ boy x ) && male x
-- isGirl	= \x -> ( female x && child x )
-- isWoman	= \x -> ( not $ isGirl x ) && female x

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
conflict	= [(Q,V),(Q,O),(O,V)]
supervision	= [(J,Q),(J,O),(O,V)]
isBoss	= pred1 $ map fst supervision
isWorker	= pred1 $ map snd supervision

twoPlacers = [
	("know",	pred2 $ acquainted ++ map swap acquainted)
	, ("have",	pred2 $ possessions ++ management ++ features)
	, ("help",	pred2 [])
	, ("said",	pred2 $ map (\x->(agent x, theme x) ) comms)
	, ("asked",	pred2 $ map (\x->(agent x, recipient x) ) comms)
	, ("talk_with_or_about",	pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(agent, theme) ) comms)
	, ("recite",	pred2 $ map ( \x -> (agent x, theme x) ) comms)
	--, ("work_where",	pred2 $ map (\x -> (patient x, agent x) ) work)
	--, ("work_as",	pred2 $ map (\x -> (patient x, location x) ) work)
	, ("studied",	\a t -> case (a,t) of
		(A,G) -> False; otherwise -> pred2 studies a t)
	]

-- stressful :: [(Entity, Entity)] -> Bool
-- stressful = \ x -> ( x `elem` [control, uncertainty, support, pressure] &&
-- 			( not $ null x ) )
threePlacers = [
	("finish",	pred3 [])
	, ("make",	pred3 $
		foldl (\ms (a,t,r,_) -> (a,t,r):(a,r,t):ms ) [] makings)
	, ("gave",	pred3 giving)
	, ("got",	pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving)
	, ("ask_about",	pred3 $ map (\x->(agent x, recipient x, theme x) ) comms)
	, ("talk_with_about",	pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
				++ comms)
	, ("told",	pred3 comms)
	, ("wanted_to_study", pred3 $ map (\(a,t) -> (a,a,t)) studies)
	]

-- control	= [(Unspec,Unspec)]
-- uncertainty	= [(Unspec,Unspec)]
-- support	= [(Unspec,E)]
--(pressurizer,pressured)
hotspots	= [(V,Q),(Q,O),(O,V),(V,O)]

fourPlacers = [
	("get",	pred4 $ map (\x -> (agent4 x, theme4 x, provider4 x, location4 x)
				) services ++
			map (\x -> (agent4 x, provider4 x, theme4 x, location4 x)
				) services )
	, ("give", pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] services)
	, ("wanted_to_make",	pred4 $ map (\(a,t,_,p) -> (a,a,p,t)) makings)
	]

possessions	= []
recruitment	= [(Unspec,Unspec,Unspec)]
appreciation	= []
family	= []
patronage	= []
acquainted	= []
studies	= []
-- (agent,theme,result,aim)
makings	= []
features	= []
services	= []
pay = pred4 $ map (\x -> (agent4 x, provider4 x, theme4 x, purpose4 x) ) services

supervisor	= pred1 $ map fst supervision
boss	= supervisor
subordinate	= pred1 $ map snd supervision
employee	= subordinate
manager = boss


wanted_to_get :: FourPlacePred
wanted_to_get	= pred4 $
	foldl ( \ps (a,t,p,l)  -> (a,a,t,p):(a,a,p,t):ps ) [] services

wanted_to_make :: FourPlacePred
wanted_to_make	= pred4 $
	map ( \x -> (agent4 x, agent4 x, theme4 x, provider4 x) ) services

offered_to_get_from :: FourPlacePred
offered_to_get_from	= pred4 []

management	= []

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


offend	= pred2 $ ( map (\x -> (agent x, recipient x) ) offenses ) ++
		( map (\x -> (theme x, recipient x) ) offenses )
anger = offend

pressurize = pred2 hotspots
gave	= pred3 giving
got	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
wanted	= got

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms
--worker	= pred1 $ map patient work

giving	= map (\(a,t,p,_) -> (a,t,p) ) services
offenses    = []
comms	= []
work	= []

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_)	= a
theme4 (_,t,_,_)	= t
recipient4 (_,_,r,_)	= r
provider4	= recipient4
location4 (_,_,_,l)	= l
purpose4	= location4
aim4	= purpose4
result4	= recipient4

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

unditrans :: FourPlacePred -> ThreePlacePred
unditrans r u v w = or ( map ( r u v w ) entities )

forgetful :: ThreePlacePred -> TwoPlacePred
forgetful r u v = or ( map ( r u v ) entities )

intransit :: TwoPlacePred -> OnePlacePred
intransit r u = or ( map ( r u ) entities )

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

-- vim: set ts=8 sts=4 sw=4 noet:
