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
    (A, "place" )
    , (B, "" )
    , (C, "price" )
    , (D, "jogging_shoes" )
    , (E, "men's_formal_shoes" )
    , (F, "women's_formal_shoes" )
    , (G, "slippers" )
    , (H, "500_nt_and_up" )
    , (I, "100-800_nt" )
    , (J, "1_200_nt_and_up" )
    , (K, "1_000_nt_and_up" )
    , (M, "miaoli" )
    , (N, "promotion" )
    , (O, "toufen" )
    , (P, "" )
    , (Q, "quanjiafu" )
    , (R, "" )
    , (S, "shoes" )
    , (T, "dr_bean" )
    , (U, "product" )
    , (W, "" )
    , (X, "yingtsailu" )
    , (Y, "money" )
    , (Z, "zhunan" )
    ]

characters :: [ (String, Entity) ]

characters = [
	( "quanjiafu",	Q )
	, ( "dr_bean",	T )
	, ( "miaoli",	M )
	, ( "toufen",	O )
	, ( "zhunan",	Z )

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
	, ("role",	pred1 [T] )
	, ("shoe_store",	pred1 [Q] )
	, ("shoes",	pred1 [S,D,E,F,G] )
	, ("jogging_shoes",	pred1 [D] )
	, ("men's_formal_shoes",	pred1 [E] )
	, ("women's_formal_shoes",	pred1 [F] )
	, ("slippers",	pred1 [G] )
	, ("teacher",	pred1 [T] )

	, ("product",	pred1 [U] )
	, ("price",	pred1 [C] )
	, ("place",	pred1 [A] )
	, ("promotion",	pred1 [N] )
	, ("person",	person )
	, ("thing",	thing )

	, ("good",	pred1 [D,E,U,C,A,N] )
	, ("bad",	pred1 [F,G] )

	, ("money",	pred1 [Y])
	, ("successful",	pred1 [Q,T])
	, ("unsuccessful",	pred1 [])
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
    , ("have",	pred2 $ possessions ++ stock ++ features )
    , ("like",	pred2 appreciation )
    , ("help",	pred2 [])
    , ("go",	pred2 $ map (\(a,_,l,_) -> (a,l) ) looking )
    , ("said",	pred2 $ map (\x->(agent x, theme x) ) comms)
    , ("asked",	pred2 $ map (\x->(agent x, recipient x) ) comms)
    , ("talk_with_or_about",	pred2 $ map (\x->(agent x, recipient x) ) comms
	++  map (\(agent,theme,recipient)->(agent, theme) ) comms)
    , ("recite",	pred2 $ map ( \x -> (agent x, theme x) ) comms)
    --, ("work_where",	pred2 $ map (\x -> (patient x, agent x) ) work)
    --, ("work_as",	pred2 $ map (\x -> (patient x, location x) ) work)
	]

threePlacers = [
    ("gave",	pred3 giving)
    , ("ask_about",	pred3 $ map (\x->(agent x, recipient x, theme x) ) comms)
    , ("talk_with_about",	pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
			    ++ comms)
    , ("have_to_buy",	have_to_buy )
    , ("told",	pred3 comms)
    ]

fourPlacers = [
	("buy",	pred4 $ foldl (\ss (a,t,p,l) -> (a,t,p,l): (a,t,l,p): ss) [] purchases)
	, ("sell", pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] purchases)
	, ("get",	pred4 $ map (\x -> (agent4 x, theme4 x, provider4 x, location4 x)
				) services ++
			map (\x -> (agent4 x, provider4 x, theme4 x, location4 x)
				) services )
	, ("give", pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] services)
	, ("pay", pred4 $ map (\x -> (agent4 x, provider4 x, theme4 x, purpose4 x) ) purchases)
    , ("wanted_to_buy", pred4 $ foldl ( \ps (a,t,p,l)  -> (a,a,t,p):(a,a,t,l):ps ) [] looking)
	]

-- (agent,theme,result,aim)
features	= []
pricing = [(Q,D,H),(Q,E,J),(Q,F,K),(Q,G,I)]
looking	= [(T,D,Q,M),(T,E,Q,M)]
purchases	= [(T,D,Q,M)]
non_purchases = looking \\ purchases
stock =  [(Q,F),(Q,G)] ++ ( map ( \(_,t,p,_) -> (p,t) ) $ looking ++ purchases )
have_to_buy = pred3 $ map (\(a,t,_,_) -> (a,Y,t) ) $ ( filter (\(a,t,r,_) -> t /= C) non_purchases ) ++ purchases
services    = []

wanted = predid4 "looking"

possessions	= [(T,Y),(Q,O),(Q,M),(Q,B),(Q,E),(Q,R),(F,B),(T,S),(J,S),(T,M),(T,E),(Unspec,E),(T,R)]

appreciation	= [(T,D),(T,E)]
knowledge	= [(T,Q),(T,M),(T,O),(T,Z)]
acquaintances	= []
comms	= [ (Someone,Y,F),(Someone,B,F),(Someone,P,F),(Someone,Y,M) ]

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
source	= recipient
instrument = recipient
origin	= theme
destination = recipient

acceptances = []
-- (seller, item, buyer)
selling	= []

said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms

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
