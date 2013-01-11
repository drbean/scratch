module Model where 

import Data.Tuple

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
	( "alex_tew",	A )
	, ( "the_million_dollar_homepage",	H )
	, ( "the_one_million_people_page",	P )
	, ( "facebook",	F )
	, ( "mark_zuckerberg",	M )

	]

classes = [ "manager", "advertisers", "advertising_space", "business_management", "radio_and_television" ]

context :: [Entity]
-- context = [A,H,P,F,M]
context = entities

namelist = map fst characters
names = map swap characters

predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid1 name = lookupPred name onePlacers where
	lookupPred n []	= error $ "no \"" ++ name ++ "\" one-place predicate."
	lookupPred n ((name,pred):is) | n == name	= pred
	lookupPred n (i:is) = lookupPred name is
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

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
	("true",	pred1 entities)
	, ("false",	pred1 [])
	, ("male",	pred1 [A,M,AA,AB,AV])
	, ("female",	pred1 [])
	, ("manager",	pred1 [A,M])
	, ("role",	pred1 [AM])
	, ("person",	person)
	, ("thing",	thing)

	, ("website",	pred1 [H,P,F])
	, ("advertising_space",	pred1 [S])
	, ("advertisers",	pred1 [AA,AB,AV])
	, ("media",	pred1 [AM])
	, ("website",	pred1 [H,P,F])
	, ("money",	pred1 [O])
	, ("good_idea",	pred1 [H,P,F])
	, ("a_good_price",	pred1 [])
	, ("business_management",	pred1 [G])

	, ("good",	pred1 [AG])
	, ("bad",	pred1 [AB])

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
boss = pred1 $ map fst management

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )
pred5 xs	= curry5 ( `elem` xs )

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

fourPlacers = [
	("buy",	pred4 $ map (\x -> (agent4 x, theme4 x, provider4 x, location4 x)
				) purchases ++
			map (\x -> (agent4 x, provider4 x, theme4 x, location4 x)
				) purchases )
	, ("sell", pred4 $ foldl (\ss (a,t,p,l) -> (l,t,a,l): (p,t,a,l): (p,t,l,a):
					ss ) [] purchases)
	, ("wanted_to_make",	pred4 $ map (\(a,t,_,p) -> (a,a,p,t)) makings)
	]

family	= []
patronage	= []
acquainted	= [(A,H),(A,P),(A,M),(A,F),(M,H),(M,P),(M,A),(M,F),(AA,H),(AB,H),(AV,H)]
studies	= [(A,G),(M,AH)]
-- (agent,theme,result,aim)
makings	= [(A,H,O,O),(A,P,Unspec,O),(M,F,O,Unspec)]
features	= [(H,S),(P,Unspec),(F,S)]
purchases	= [(AA,S,A,H),(AB,S,A,H),(AV,S,A,H)]
pay = pred4 $ map (\x -> (agent4 x, provider4 x, theme4 x, purpose4 x) ) purchases

wanted = predid4 "buy"

decided_to_make :: ThreePlacePred
decided_to_make = predid3 "make"

wanted_to_buy :: FourPlacePred
wanted_to_buy	= pred4 $
	foldl ( \ps (a,t,p,l)  -> (a,a,t,p):(a,a,p,t):ps ) [] purchases

wanted_to_make :: FourPlacePred
wanted_to_make	= pred4 $
	map ( \x -> (agent4 x, agent4 x, theme4 x, provider4 x) ) purchases

offered_to_buy_from :: FourPlacePred
offered_to_buy_from	= pred4 []

possessions	= [(A,H),(M,F),(M,O),(AA,O),(AB,O),(AV,O)]
management	= [(M,F)]

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
source	= recipient
instrument = recipient
origin	= theme
destination = recipient



--worker	= pred1 $ map patient work

giving	= map (\(a,t,p,_) -> (a,t,p) ) purchases
comms	= [ (A,AM,H),(A,M,H),(M,A,F) ]
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

unditrans :: FourPlacePred -> ThreePlacePred
unditrans r u v w = or ( map ( r u v w ) entities )

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

-- vim: set ts=8 sts=4 sw=4 noet:
