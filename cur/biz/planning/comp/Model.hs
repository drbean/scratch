module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	= A | B | C | D | E | F | G 
		| H | I | J | K | L | M | N 
		| O | P | Q | R | S | T | U 
		| V | W | X | Y | Z 
		| Someone | Something | Unspec
		| S1 | S2 | S3 | S4 | S5
		| S6 | S7 | S8 | S9 | S10
		| S11 | S12 | S13 | S14 | S15
		| S16 | S17 | S18 | S19 | S20
		| X1 | X2 | X3 | X4 | X5
		| X6 | X7 | X8 | X9 | X10
		| X11 | X12 | X13 | X14 | X15
		| X16 | X17 | X18 | X19 | X20
		deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 

entity_check :: [ (Entity, String) ]
entity_check =  [
    (A, "autonomy" )
    , (B, "board" )
    , (C, "clear_and_simple_idea" )
    , (D, "" )
    , (E, "english" )
    , (F, "framework" )
    , (G, "group" )
    , (H, "" )
    , (I, "ingredients_for_success" )
    , (J, "" )
    , (K, "" )
    , (L, "loser" )
    , (M, "member" )
    , (N, "innovation" )
    , (O, "ownership" )
    , (P, "points" )
    , (Q, "question" )
    , (R, "" )
    , (S, "student" )
    , (T, "dr_bean" )
    , (U, "" )
    , (V, "" )
    , (W, "winner" )
    , (X, "compcomp_activity" )
    , (Y, "answer" )
    , (Z, "" )
    ]

characters :: [ (String, Entity) ]

characters = [
	( "dr_bean",	T )

	]

classes :: [String]
classes = []

context :: [Entity]
context = [M,T,F,K,Someone]

namelist :: [String]
namelist = map fst characters
names :: [(Entity, String)]
names = map swap characters

predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid4 :: String -> FourPlacePred
predid1 name = lookup1 name onePlacers where
	lookup1 name []	= error $ "no '" ++ name ++ "' one-place predicate."
	lookup1 name ((n,p):_) | n == name	= p
	lookup1 name (i:is) = lookup1 name is
predid2 name = lookup2 name twoPlacers where
	lookup2 n []	= error $ "no '" ++ name ++ "' two-place predicate."
	lookup2 n ((name,pred):is) | n == name	= pred
	lookup2 n (i:is) = lookup2 name is
predid3 name = lookupPred name threePlacers where
	lookupPred n []	= error $ "no '" ++ name ++ "' three-place predicate."
	lookupPred n ((name,pred):is) | n == name      = pred
	lookupPred n (i:is) = lookupPred name is
predid4 name = lookupPred name fourPlacers where
	lookupPred n []	= error $ "no '" ++ name ++ "' four-place predicate."
	lookupPred n ((name,pred):is) | n == name     = pred
	lookupPred n (i:is) = lookupPred name is
predid5 name = lookupPred name fivePlacers where
	lookupPred n []	= error $ "no '" ++ name ++ "' five-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
	("true",	pred1 entities )
	, ("false",	pred1 [] )
	, ("male",	pred1 [T] )
	, ("female",	pred1 [] )
	, ("role",	pred1 [] )
	, ("teacher",	pred1 [T] )
	, ("student",	pred1 [S1,S2,S3,S4,S11] )
	, ("group",	pred1 [G] )
	, ("teacher",	pred1 $ map ( \(t,_,_,_) -> t ) schooling )

	, ("english",	pred1 [E] )

	, ("question",	pred1 [Q] )
	, ("answer",	pred1 [Y] )
	, ("person",	person )
	, ("thing",	thing )

	, ("good",	pred1 [C,A,O,N,Y] )
	, ("bad",	pred1 [F,G] )

	, ("innovative",	pred1 [X1] )
	, ("compcomp_activity",	pred1 [X1,X11])
	, ("ingredients_for_success",	pred1 [C,A,O,N])
	, ("clear_and_simple_idea",	pred1 [C])
	, ("autonomy",	pred1 [A])
	, ("ownership",	pred1 [O])
	, ("innovation",	pred1 [X,X1,X11,X3,X4])
	, ("framework",	predid1 "ingredients_for_success" )
	, ("activity",	pred1 [X,X1,X11,X3,X4])
	, ("successful",	pred1 [X1])
	, ("unsuccessful",	pred1 [X11])

	, ("winner",	pred1 [W])
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
    ("know",	pred2 $ knowledge ++ acquaintances ++ map swap acquaintances)
    , ("have",	pred2 $ (map (\(t,_,_,s) -> (t,s) ) schooling ) ++ possessions 
	++ (map (\(s,c,_,_) -> (s,c)) $ filter (\(_,c,_,_) -> or [(predid1 "answer" c), (predid1 "question" c)]) comms ) )
    , ("like",	pred2 $ map (\(a,t,r) -> (a,t)) appreciation)
    , ("help",	pred2 [])
    , ("speak",	pred2 [])
    , ("listen",	pred2 [])
    , ("read",	pred2 [])
    , ("write",	pred2 [])
    , ("said",	pred2 $ map (\x->(agent4 x, theme4 x) ) comms)
    , ("asked",	pred2 $ map (\x->(agent4 x, recipient4 x) ) comms)
    , ("talk_with_or_about",	pred2 $ map (\x->(agent4 x, recipient4 x) ) comms
	++  map (\(agent,theme,recipient,_)->(agent, theme) ) comms)
    , ("recite",	pred2 $ map ( \x -> (agent4 x, theme4 x) ) comms)
	]

-- (agent,theme,result,aim)
features	= [(X1,A)]
services    = []

wanted = predid4 "looking"

possessions	= [(T,A),(S2,A),(T,X2),(S1,Q)]

-- (appreciator, performance, actor)
appreciation	= [(S1,X1,S),(T,E,S),(T,Q,S4),(T,Q,T)]
knowledge	= []
acquaintances	= []

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
mode = theme
location = recipient
source	= recipient
instrument = recipient
origin	= theme
destination = recipient

threePlacers = [
    ("gave",	pred3 giving)
    , ("ask_about",	pred3 $ map (\x->(agent4 x, recipient4 x, theme4 x) ) comms)
    , ("talk_with_about",	pred3 $ foldl (\cs (s,c,l,m) ->(s,l,c): (s,c,l): (s,l,m): cs) [] comms )
    , ("like_to_ask",	pred3 $ map (\(a,t,r) -> (a,r,t)) appreciation )
    ]

acceptances = []
-- (seller, item, buyer)
selling	= []

said	= pred2 $ map (\x->(agent4 x, theme4 x) ) comms
asked	= pred2 $ map (\x->(agent4 x, recipient4 x) ) comms
ask_about = pred3 $ map (\x->(agent4 x, recipient4 x, theme4 x) ) comms
talked	= pred2 $ map (\x->(agent4 x, recipient4 x) ) comms
              ++  map (\(agent,theme,recipient,_)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent4 x, recipient4 x, theme4 x) ) comms

fourPlacers = [
	("get",	pred4 $ map (\x -> (agent4 x, theme4 x, provider4 x, location4 x)
				) services ++
			map (\x -> (agent4 x, provider4 x, theme4 x, location4 x)
				) services )
	, ("give",	pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] services)
	, ("ask",   pred4 $foldl (\cc (a,t,r,m) -> (a,t,r,m): (a,r,t,m): cc) [] comms)
	, ("have_to_ask",	pred4 $ map (\(_,t,r,l) -> (r,r,t,l) ) directives )
	, ("wanted_to_talk",	pred4 $ foldl (\cc (a,t,r,p) -> (a,t,r,p): (a,r,t,p): (a,r,p,t): cc) [] goals)
	, ("wanted_to_ask",	pred4 $ foldl (\cc (a,t,r,p) -> (a,t,r,p): (a,r,t,p): (a,r,p,t): cc) [] goals)
	, ("wanted_to_answer",	pred4 $ foldl (\cc (a,t,r,p) -> (a,t,r,p): (a,r,t,p): (a,r,p,t): cc) [] goals)
	]

-- (teacher,activity,group,student)
schooling   = [(T,X1,G,S1),(T,X11,G,S11)]
recite = pred2 $ map ( \x -> (agent4 x, theme4 x) ) comms
giving	= map (\(a,t,p,_) -> (a,t,p) ) services
-- (speaker,content,listener,mode)
comms	= [(T,Unspec,S1,E),(T,Q,S2,E),(S1,Y,S2,E)]
-- (instigator,act,agent,situation)
directives  = [(T,Q,S3,X3)]
-- (planner,situation,achiever,goal)
goals	= [(T,X4,S4,E),(T,X1,T,Q),(T,X1,S1,Q)]

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_)	= a
theme4 (_,t,_,_)	= t
recipient4 (_,_,r,_)	= r
provider4	= recipient4
location4 (_,_,_,l)	= l
mode4	= location4
purpose4	= location4
aim4	= purpose4
result4	= recipient4


fivePlacers = [
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
