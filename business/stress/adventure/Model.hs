module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

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
	( "ellen_macarthur",	M ),
	( "dr_bean",	T ),
	( "steve_fossett",	F ),
	( "cusp",	K )

	]

classes :: [String]
classes = ["lack_of_support","uncertainty","lack_of_control","pressure"]

context :: [Entity]
context = [M,T,F,K,Someone]

plane = predid1 "poweredcraft"

framework	= pred1 [K]
useful	= framework

namelist :: [String]
namelist = map fst characters
names :: [(Entity, String)]
names = map swap characters


predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid4 :: String -> FourPlacePred
predid1 name = lookup1 name onePlacers where
	lookup1 name []	= error $ "no \"" ++ name ++ "\" one-place predicate."
	lookup1 name ((n,p):_) | n == name	= p
	lookup1 name (i:is) = lookup1 name is
predid2 name = lookup2 name twoPlacers where
	lookup2 n []	= error $ "no \"" ++ name ++ "\" two-place predicate."
	lookup2 n ((name,pred):is) | n == name	= pred
	lookup2 n (i:is) = lookup2 name is
predid3 name = lookupPred name threePlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" three-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is
predid4 name = lookupPred name fourPlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" four-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is
predid5 name = lookupPred name fivePlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" five-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
	("true",	pred1 entities )
	, ("false",	pred1 [] )
	, ("male",	pred1 [F,T] )
	, ("female",	pred1 [M] )
	, ("ambitious",	pred1 [F,M] )
	, ("leader",	pred1 $ map snd leadership )
	, ("adventurer",	pred1 $ map snd leadership )
	, ("financial_trader",	pred1 [F] )
	, ("teacher",	pred1 [T] )
	, ("team",	pred1 [Someone] )
	, ("role",	pred1 [] )
	, ("person",	person )
	, ("thing",	thing )

    , ("lack_of_control",	pred1 [C] )
    , ("uncertainty",	pred1 [U] )
    , ("lack_of_support",	pred1 [S] )
    , ("pressure",	pred1 [P] )
    , ("stress",	pred1 [R] )
    , ("stressful",	pred1 [C,U,S,P] )

	, ("good",	pred1 [] )
	, ("bad",	pred1 [C,U,S,P,R] )

    , ("balloon",   pred1 [B] )
    , ("glider",    pred1 [G] )
    , ("plane",	pred1 [P] )
    , ("aircraft",  pred1 [B,P,G] )
    , ("boat",	pred1 [Y] )

    , ("world",	pred1 [W] )
    , ("non-stop",	pred1 [N] )
    , ("single-handed",	pred1 [O] )

	]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool
type FourPlacePred	= Entity -> Entity -> Entity -> Entity -> Bool
type FivePlacePred	= Entity -> Entity -> Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

person, thing :: OnePlacePred
person	= \ x -> (predid1 "male" x || predid1 "female" x || predid1 "role" x || x == Someone)
thing	= \ x -> (x == Unspec || x == Something || not ( person x ) )

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )
pred5 xs	= curry5 ( `elem` xs )

twoPlacers :: [(String, TwoPlacePred)]
twoPlacers = [
    ("know",	pred2 $ acquainted ++ map swap acquainted)
    , ("have",	pred2 $ possessions ++ leadership ++ features)
    , ("felt_stress",	pred2 $ map (\x -> (recipient x, agent x) ) reactions )
    , ("cause_stress",	pred2 $ map (\x -> (agent x, recipient x) ) reactions )
    , ("pressurize",	pred2 [])
    , ("help",	pred2 [])
    , ("said",	pred2 $ map (\x->(agent x, theme x) ) comms)
    , ("asked",	pred2 $ map (\x->(agent x, recipient x) ) comms)
    , ("talk_with_or_about",	pred2 $ map (\x->(agent x, recipient x) ) comms
	++  map (\(agent,theme,recipient)->(agent, theme) ) comms)
    , ("recite",	pred2 $ map ( \x -> (agent x, theme x) ) comms)
    --, ("work_where",	pred2 $ map (\x -> (patient x, agent x) ) work)
    --, ("work_as",	pred2 $ map (\x -> (patient x, location x) ) work)
	]

reactions = [(C,R,T),(U,R,T),(C,R,F),(U,R,F),(P,R,F),(C,R,M),(U,R,M),(P,R,M)]

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
hotspots	= []

fourPlacers = [
	("get",	pred4 $ map (\x -> (agent4 x, theme4 x, provider4 x, location4 x)
				) services ++
			map (\x -> (agent4 x, provider4 x, theme4 x, location4 x)
				) services )
	, ("give", pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] services)
	, ("wanted_to_make",	pred4 $ map (\(a,t,_,p) -> (a,a,p,t)) makings)
	]

-- circumnavigator, vehicle, accomplishment
circumnavigations	= [(S,B),(S,P),(S,Y),(E,Y)]
possessions	= [(F,Y),(F,G),(F,B),(F,P),(M,Y)]
recruitment	= []
support	= [(Someone,M),(Someone,F)]
leadership	= map swap support
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

help	= support
supporter	= pred1 $ map fst support
supported	= pred1 $ map snd support
leader	= supported
team_member	= supporter

circumnavigate = pred2 circumnavigations
fly_around_in	= pred3 $ map (\x -> (fst x,W,snd x) ) $ filter (\x -> snd x == B || snd x == P)
			circumnavigations
sail_around_in	= pred3 $ map (\x -> (fst x,W,snd x) ) $ filter (\x -> snd x == Y )
			circumnavigations
fly_around	= forgetful3 fly_around_in
sail_around	= forgetful3 sail_around_in

looking	= []
have	= pred2 $ possessions ++ support
		++ ( map swap $ support )
		++ ( map (\x->(recipient x, theme x) ) giving )
		++ ( map (\x->(agent x,J) ) working )
		++ ( map (\x->(agent x, patient x) ) recruitment )
		++ ( map (\x->(agent x, location x) ) recruitment )
knowledge	= []
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
appreciate	= pred2 appreciation
-- visit	= pred2 $ map (\x -> (patient x, recipient x) ) recruitment
interview	= pred2 $ map (\x -> (agent x, patient x) ) recruitment
-- greet	= interview
look_at	= pred2 $ looking

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)
curry5 f x y z w v	= f (x,y,z,w,v)

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
volunteering = []
comms	= [ (Someone,Y,F),(Someone,B,F),(Someone,P,F),(Someone,Y,M) ]
offenses	= []
acceptances = []
-- (seller, item, buyer)
selling	= []
--(killer,killed,instrument)
--(putter,theme,location)
--(agent,theme,location)
looking_back	= [(Q,Unspec,Unspec)]

worker	= pred1 $ map agent working
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
volunteer_at	= pred2 $ map (\x -> (agent x, location x) ) volunteering
look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back
said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
offend_with	= pred3 offenses
offend	= pred2 $ ( map (\x -> (agent x, recipient x) ) offenses ) ++
		( map (\x -> (theme x, recipient x) ) offenses )
anger = offend

pressurize = pred2 hotspots

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms
giving	= map (\(a,t,p,_) -> (a,t,p) ) services

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_)	= a
theme4 (_,t,_,_)	= t
recipient4 (_,_,r,_)	= r
provider4	= recipient4
location4 (_,_,_,l)	= l
purpose4	= location4
aim4	= purpose4
result4	= recipient4

sailing    = [(F,W,Unspec,N),(M,W,O,N)]
ballooning	= [(F,W,O,N)]
plane_flights	= [(F,W,O,N)]

fivePlacers = [
	("sailed", pred5 $ foldl (\ss (a,d,s1,s2) ->
	    (a,d,s1,s2,Y) : (a,Y,d,s1,s2) : (a,d,Y,s1,s2) : ss ) [] sailing)
	, ("flew", pred5 $ foldl (\ss (a,d,s1,s2) ->
	    (a,d,s1,s2,P) : (a,P,d,s1,s2) : (a,d,P,s1,s2) : ss ) [] plane_flights
			++ foldl (\ss (a,d,s1,s2) ->
	    (a,d,s1,s2,B) : (a,B,d,s1,s2) : (a,d,B,s1,s2) : ss ) [] ballooning)
	]


agent5, theme5, recipient5, location5 :: (Entity,Entity,Entity,Entity,Entity) -> Entity
agent5 (a,_,_,_,_)	= a
theme5 (_,t,_,_,_)	= t
destination5 = theme5
recipient5 (_,_,r,_,_)	= r
provider5	= recipient5
result5	= recipient5
style5	= recipient5
feature5 (_,_,_,f,_)	= f
location5 (_,_,_,_,l)	= l
purpose5	= location5
aim5	= purpose5
vehicle5	= location5

forgetful5 :: FivePlacePred -> FourPlacePred
forgetful5 r u v w t = or ( map ( r u v w t ) entities )

forgetful4 :: FourPlacePred -> ThreePlacePred
forgetful4 r u v w = or ( map ( r u v w ) entities )

forgetful3 :: ThreePlacePred -> TwoPlacePred
forgetful3 r u v = or ( map ( r u v ) entities )

forgetful2 :: TwoPlacePred -> OnePlacePred
forgetful2 r u = or ( map ( r u ) entities )

passivize :: TwoPlacePred -> OnePlacePred
passivize r	= \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r	= \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

-- vim: set ts=8 sts=4 sw=4 noet:
