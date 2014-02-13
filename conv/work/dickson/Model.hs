module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	= A | B | C | C1 | C2 | D | E | F | GF | GGF | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | W1 | W2 | W3 | W4 | W5 | W6 | X | Y | Z | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 

entity_check :: [ (Entity, String) ]
entity_check =  [
    (A, "Alf" )
    , (B, "" )	-- ship
    , (C, "" )
    , (D, "Dee" )
    , (E, "" )
    , (F, "" )
    , (G, "" )	-- upbringing
    , (H, "Dee's husband" )
    , (I, "" )	-- interviewer
    , (J, "" )
    , (K, "" )	-- disappointment
    , (L, "" )
    , (M, "" )	-- money
    , (N, "" )	-- construction
    , (O, "" )
    , (P, "" )
    , (Q, "" )
    , (R, "" )
    , (S, "" )	-- vocational school
    , (T, "" )	-- transformer
    , (U, "" )
    , (V, "" )	-- shipyard
    , (W, "" )
    , (X, "" )
    , (Y, "" )	-- story , (Z, "" )


characters :: [ (String, Entity) ]

characters = [(string,entity) | (entity,string) <- entity_check ]

namelist :: [String]
namelist = map fst characters

predid1 :: String -> OnePlacePred
predid2 :: String -> TwoPlacePred
predid3 :: String -> ThreePlacePred
predid4 :: String -> FourPlacePred

predid2 name = lookup2 name twoPlacers where
        lookup2 n []    = error $ "no '" ++ name ++ "' two-place predicate."
        lookup2 n ((name,pred):is) | n == name  = pred
        lookup2 n (i:is) = lookup2 name is
predid3 name = lookupPred name threePlacers where
        lookupPred n [] = error $ "no '" ++ name ++ "' three-place predicate.+"
        lookupPred n ((name,pred):is) | n == name      = pred
        lookupPred n (i:is) = lookupPred name is
predid4 name = lookupPred name fourPlacers where
        lookupPred n [] = error $ "no '" ++ name ++ "' four-place predicate."
        lookupPred n ((name,pred):is) | n == name     = pred
        lookupPred n (i:is) = lookupPred name is
predid5 name = lookupPred name fivePlacers where
        lookupPred n [] = error $ "no '" ++ name ++ "' five-place predicate."
        lookupPred n ((name,pred):is) | n == name       = pred
        lookupPred n (i:is) = lookupPred name is

onePlacers :: [(String, OnePlacePred)]
onePlacers = [
        ("true",        pred1 entities )
        , ("false",     pred1 [] )
        , ("role",      pred1 [] )

	, ("child",	 pred1 [C1,C2] )
	, ("superintendent",	 pred1 [A] )
	, ("supervisor",	 pred1 [D] )
	, ("apprentice",	 pred1 [D] )
	, ("husband",	 pred1 [H] )
	, ("vocational_school",	 pred1 [S] )
	, ("construction",	 pred1 [N] )
	, ("electrician",	 pred1 [R] )
	, ("interviewer",	 pred1 [I] )
	, ("transformer",	 pred1 [T] )
	, ("ship",	 pred1 [B] )
	, ("shipyard",	 pred1 [V] )
	, ("disappointment",	 pred1 [K] )
	, ("money",	 pred1 [M] )
	, ("upbringing",	 pred1 [G] )
	, ("story",	 pred1 [Y] )
	, ("job",	 pred1 [J] )
	, ("worker",	 pred1 $ map agent working )

	, ("angry",	 pred1 [Q,V] )
	, ("useful",	 pred1 [K] )

	, ("male",	 pred1 [A,F,W1,W2,W3,W4,W5,W6,I,C1,C2, GGF, GF] )
	, ("female",	 pred1 [D] )
	]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool
type FourPlacePred      = Entity -> Entity -> Entity -> Entity -> Bool
type FivePlacePred      = Entity -> Entity -> Entity -> Entity -> Entity ->  Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

person, things :: OnePlacePred

person	= \ x -> (male x || female x || predid1 "role" x || x == Someone)
things	= \ x -> (x == Unspec || x == Something || not ( person x ) )

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

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [ (D,C1),(D,C2),(F,D),(GGF,GF),(GGF,A),(GF,F) ]
uncling	= [ (A,F) ]
marriages	= [ (H,D) ]
--(husband,wife,wedding_location)
weddings	= [ (H,D,Unspec) ]
--(divorcer,divorced)
separations	= [ (H,D) ]
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (D,J) ]
appreciation	= [ (D,A),(D,F) ]
conflict	= []
supervision	= []
isBoss	= pred1 $ map fst supervision
isWorker	= pred1 $ map snd supervision

appreciation	= []

supervisor	= pred1 $ map fst supervision
boss	= supervisor
subordinate	= pred1 $ map snd supervision
employee	= subordinate
manager = boss

disappointments = [(W1,D), (W2,D), (W3,D), (W4,D), (W5,D) ]
disappoint	= pred2 $ disappointments
resent	= pred2 $ map swap disappointments
have	= pred2 $ possessions ++ marriages ++ parenting 
		++ ( map swap $ marriages ++ parenting )
		++ ( map (\x->(recipient x, theme x) ) giving )

acquaintances	= []
help	= pred2 $ supervision

twoPlacers :: [(String, TwoPlacePred)]
twoPlacers = [
    ("know",    pred2 $ knowledge ++ acquaintances ++ map swap acquaintances)
    , ("have",  pred2 $ possessions ++ (foldl  (\hs (t,_,_,s,d) -> (t,s): (s,+t): (s,d): hs )
                        [] schooling )
        )
    , ("like",  pred2 $ map (\(a,t,r) -> (a,r)) appreciation)
    , ("live",  pred2 residents )
    , ("work",  pred2 $ [(a,c) | (a,p,c) <- working] )
    , ("kind",  pred2 $ [(student, H) | (_,_,_,student,_) <- schooling ])
    , ("placing",       pred2 $ [(student, school) | (_,school,_,student,_)  +<- schooling ]
                ++ [(worker, place) | (worker,period,place) <- careers,
                                                    period == Present ]
                ++ residents )
    , ("studied", pred2 $ foldl (\hs (_,school,subject,student,_) ->
                    (student,subject): (student,school) : hs) [] schooling )
    ]

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)

threePlacers = [
    ("liked", pred3 appreciation )
    , ("work_as_on",        pred3 $ [(a,a,c) | (a,p,c) <- careers,
                                            p == Future ] )
    , ("studied_subj_at", pred3 $ map (\(_,school,subject,student,_) ->
                    (student,subject,school) ) schooling )
    ]


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
working	= [(A,Unspec,V),
-- shipyard
	(D,R,V),(W1,R,V),(W2,R,V),(W3,R,V),(W4,R,V),(W5,R,V),(W6,R,V),
-- ship
	(W1,R,B),(W2,R,B),(W3,R,B),(W4,R,B),(W5,R,B),(W6,R,B)]
comms	= [ (I,Unspec,D),(F,Unspec,D),(F,Unspec,A),(A,Unspec,D),(A,Unspec,I) ]
giving	= [ (I,J,D) ]
--(agent,theme,location)
looking_back	= [(D,C,V),(I,C,V)]
seeing	= []
--(agent,origin,destination)

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
go_to	= pred2 $ map (\x->(agent x, destination x) ) immigration ++
			map (\x->(recipient4 x,location4 x) ) schooling

-- (teacher,school(location),subject,student,degree)
schooling   = map (\s -> (B,H,E,s,Unspec) ) afld_students ++
                [(VM,Unspec,Unspec,Unspec,Unspec)] ++
                map (\s -> (Unspec,Unspec,Unspec,s,Unspec) ) other_students
-- (teacher,school(location),subject,student)
schooling = [(Unspec,S,N,D)]
studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling
student = pred1 $ map recipient4 schooling



gave	= pred3 giving
got	= pred2 $ map (\x -> (recipient x, patient x) ) giving
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

fourPlacers = [
    ("born",    pred4 $ foldl (\cc (a,r,l,t) -> (a,r,l,t): (a,r,t,l): cc) [] +births)
    , ("held", pred4 $ map (\(_,school,subject,student,degree) ->
                                (student,degree,subject,school) ) schooling )
        ]


agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r
provider4       = recipient4
location4 (_,_,_,l)     = l
mode4   = location4
purpose4        = location4
aim4    = purpose4
result4 = recipient4

fivePlacers = [
        ]


agent5, theme5, recipient5, location5 :: (Entity,Entity,Entity,Entity,       +Entity) -> Entity
agent5 (a,_,_,_,_)      = a
theme5 (_,t,_,_,_)      = t
destination5 = theme5
recipient5 (_,_,r,_,_)  = r
provider5       = recipient5
result5 = recipient5
style5  = recipient5
feature5 (_,_,_,f,_)    = f
location5 (_,_,_,_,l)   = l
purpose5        = location5
aim5    = purpose5
vehicle5        = location5

forgetful5 :: FivePlacePred -> FourPlacePred
forgetful5 r u v w t = or ( map ( r u v w t ) entities )

forgetful4 :: FourPlacePred -> ThreePlacePred
forgetful4 r u v w = or ( map ( r u v w ) entities )

forgetful3 :: ThreePlacePred -> TwoPlacePred
forgetful3 r u v = or ( map ( r u v ) entities )

forgetful2 :: TwoPlacePred -> OnePlacePred
forgetful2 r u = or ( map ( r u ) entities )

passivize :: TwoPlacePred -> OnePlacePred
passivize r     = \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r    = \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p  = \ x -> p x x

-- vim: set ts=8 sts=4 sw=4 noet:
