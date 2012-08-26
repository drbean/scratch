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


characters :: [ (String, Entity) ]

characters = [
	( "fast_track",	F ),
	( "barbara",	B ),
	( "eva",	E ),
	( "tadeusz",	T ),
	( "dr_bean",	D )

	]

male, female :: OnePlacePred

female	= pred1 [B,E]
male	= pred1 [T,D]
role	= pred1 [R,P,S]
child	= pred1 []

company	= pred1 [F]
candidate	= pred1 [B,T,E,D]
sales_representative	= pred1 [R]
regional_manager	= pred1 [P]
sales_manager	= pred1 [S]
secretary	= pred1 []
subject	= pred1 [M,G,H]	-- marketing, engineering, history
job	= pred1 [J]
story	= pred1 [Y]
thirty	= pred1 [B]
fifty_two	= pred1 [T]
forty_two	= pred1 [E]
successful	= pred1 [B,T,E]
-- barbara
energetic	= pred1 [B]
confident	= energetic
aggressive	= energetic
ambitious	= energetic
difficult_to_work_with	= energetic
strong	= energetic
-- tadeusz
calm	= pred1 [T]
relaxed	= calm
hard_working	= calm
practical	= calm
reliable	= calm
-- eva
quiet	= pred1 [E]
nervous	= quiet

possessions	= []
appreciation	= []
identities	= [(B,P),(T,R),(E,P)]
supervision	= [(T,Unspec)]
-- appearances	= [(B,Strong)]

--(site(company),worker,job)
recruitment	= [(F,B,R),(F,T,P),(F,E,R),(F,Unspec,S)]
comms	= [ (B,Y,Unspec),(A,Y,Unspec),(E,Y,Unspec) ]
giving	= []

-- (teacher,school(location),subject,student)
schooling = [(Unspec,Unspec,M,B),(Unspec,Unspec,G,T),(Unspec,Unspec,H,E)]

namelist = map fst characters

names = map swap characters

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

people, things :: OnePlacePred

people	= \ x -> (male x || female x || role x || x == Unspec)
things	= \ x -> (x == Unspec || not ( people x ) )

boy	= \x -> male x && child x
isMan	= \x -> ( not $ boy x ) && male x
isGirl	= \x -> ( female x && child x )
isWoman	= \x -> ( not $ isGirl x ) && female x
interviewee = pred1 $ map patient recruitment
boss = pred1 $ map fst supervision

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

have	= pred2 $ possessions ++ supervision
		++ ( map (\x->(patient x,J) ) recruitment )
		++ ( map (\x->(agent x, location x) ) recruitment )

knowledge	= []
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
appreciate	= pred2 appreciation
visit	= pred2 $ map (\x -> (patient x, recipient x) ) recruitment
interview	= pred2 $ map (\x -> (agent x, patient x) ) recruitment
greet	= interview

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

worker	= pred1 $ map patient recruitment
work_where	= pred2 $ map (\x -> (patient x, agent x) ) recruitment
work_as = pred2 $ map (\x -> (patient x, location x) ) recruitment
said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

gave	= pred3 giving
got	= pred2 $ map (\x -> (recipient x, patient x) ) giving
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

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
