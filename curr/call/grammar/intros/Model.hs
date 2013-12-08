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

entity_check :: [ (Entity, String) ]
entity_check =  [
    (A, "alex" )
    , (B, "dr_bean" )
    , (C, "cindy" )
    , (D, "dave" )
    , (E, "applied_foreign_languages" )
    , (F, "" )
    , (G, "" )
    , (H, "minghsin_university" )
    , (I, "hello_kitty" )
    , (J, "jeff" )
    , (K, "kelly" )
    , (L, "avril_lavigne" )
    , (M, "mindy" )
    , (N, "neil" )
    , (O, "" )
    , (P, "the_color_pink" )
    , (Q, "mi_mi" )
    , (R, "rena" )
    , (S, "shane" )
    , (T, "" )
    , (U, "america" )
    , (V, "vicky" )
    , (W, "taiwan" )
    , (X, "hsinchu" )
    , (Y, "" )
    , (Z, "music" )
    ]

characters :: [ (String, Entity) ]

characters = [
	("alex", A)
	, ("dr_bean", B)
	, ("cindy", C)
	, ("dave", D)
	, ("applied_foreign_languages", E)
	, ("", F)
	, ("", G)
	, ("minghsin university", H)
	, ("hello kitty", I)
	, ("jeff", J)
	, ("kelly", K)
	, ("", L)
	, ("mindy", M)
	, ("neil", N)
	, ("", O)
	, ("the color pink", P)
	, ("mi-mi", Q)
	, ("rena", R)
	, ("shane", S)
	, ("", T)
	, ("america", U)
	, ("vicky", V)
	, ("taiwan", W)
	, ("xinzhu", X)
	, ("", Y)
	, ("", Z)

	]

classes :: [String]
classes = []

context :: [Entity]
context = []

namelist :: [String]
namelist = map fst characters
names :: [(Entity, String)]
names = map swap characters

predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid4 :: String -> FourPlacePred

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

students    = [A,C,D,J,K,M,N,R,S,V]

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
	("true",	pred1 entities )
	, ("false",	pred1 [] )
	, ("role",	pred1 [] )
	, ("teacher",	pred1 [B] )
	, ("student",	pred1 students )
	, ("worker",	pred1 [ w | (w,period,_) <- careers,
				    period == Present ] )

	, ("male",	pred1 [B,A,D,J,N,S] )
	, ("female",	pred1 [C,K,M,R,V] )
	, ("thing",	thing )
	, ("cat",	pred1 [N] )

	, ("old",	pred1 [B] )
	, ("reserved",	pred1 [M,R] )
	, ("reserved",	pred1 [M,R] )
	, ("outgoing",	pred1 [R] )

	, ("applied_foreign_languages",	pred1 [E])
	]

predid1 "optimistic"	= predid1 "outgoing"
predid1 "shy"	= predid1 "reserved"
predid1 name = lookup1 name onePlacers where
	lookup1 name []	= error $ "no '" ++ name ++ "' one-place predicate."
	lookup1 name ((n,p):_) | n == name	= p
	lookup1 name (i:is) = lookup1 name is

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

-- (agent,theme,result,aim)
features	= []
services    = []

knowledge	= []
acquaintances	= [(M,J)]
residents   = [(A,X),(M,X),(K,X)]

twoPlacers :: [(String, TwoPlacePred)]
twoPlacers = [
    ("know",	pred2 $ knowledge ++ acquaintances ++ map swap acquaintances)
    , ("have",	pred2 $ (foldl  (\hs (t,_,_,s,d) -> (t,s): (s,t): (s,d): hs )
			[] schooling )
	)
    , ("like",	pred2 $ map (\(a,t,r) -> (a,t)) appreciation)
    , ("resident",	pred2 residents )
    , ("kind",	pred2 $ map ( \(_,_,subject,student,degree) -> (degree,subject ) )
		    schooling)
    , ("wanted_to_work", pred2 $ [(a,c) | (a,p,c) <- careers,
					    p == Future ] )
	]


curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)
curry5 f x y z w v	= f (x,y,z,w,v)

-- (appreciator, performance, actor)
appreciation	= [(R,Unspec,Z),(R,Unspec,P),(R,Unspec,I),(R,Unspec,Q),(R,Unspec,L)]
data Period	= Present | Future
		deriving (Eq,Show,Bounded,Enum,Ord)
periods :: [Period]
periods	=  [minBound..maxBound] 
-- (agent, status, ie present or future, career)
careers	    = [(R,Future,U)]

threePlacers = [
    ("liked", pred3 appreciation )
    ]

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

acceptances = []
-- (seller, item, buyer)
selling	= []

said	= pred2 $ map (\x->(agent4 x, theme4 x) ) comms
asked	= pred2 $ map (\x->(agent4 x, recipient4 x) ) comms
ask_about = pred3 $ map (\x->(agent4 x, recipient4 x, theme4 x) ) comms
talked	= pred2 $ map (\x->(agent4 x, recipient4 x) ) comms
              ++  map (\(agent,theme,recipient,_)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent4 x, recipient4 x, theme4 x) ) comms

-- (teacher,school(location),subject,student,degree)
schooling   = map ( \x -> (Unspec,H,E,x,Unspec) ) students
recite = pred2 $ map ( \x -> (agent4 x, theme4 x) ) comms
giving	= map (\(a,t,p,_) -> (a,t,p) ) services
-- (speaker,content,listener,mode)
comms	= []
-- (instigator,act,agent,situation)
directives  = []
-- (planner,situation,achiever,goal)
goals	= []
-- (mother,baby,place,year)
births	= [(Unspec,A,X,Unspec),(Unspec,K,X,Unspec),(Unspec,M,X,Unspec)]

fourPlacers = [
    ("born",	pred4 $ foldl (\cc (a,r,l,t) -> (a,r,l,t): (a,r,t,l): cc) [] births)
    , ("held", pred4 $ map (\(_,school,subject,student,degree) ->
				(student,degree,subject,school) ) schooling )
	
	]

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