module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

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
	( "germany",	Z ),
	( "the_united_states",	U ),
	( "poland",	K ),
	( "fast-track",	F ),
	( "barbara",	B ),
	( "eva",	E ),
	( "tadeusz",	T ),
	( "dr_bean",	D )

	]

male, female :: OnePlacePred

polish	= pred1 [B,T]
german	= pred1 [E]
american	= pred1 [F]

female	= pred1 [B,E]
male	= pred1 [T,D]
role	= pred1 [R,P,S]
child	= pred1 []

company	= pred1 [F]
candidate	= pred1 [B,T,E,D]
sales_representative	= pred1 [P]
regional_manager	= pred1 [R]
sales_manager	= pred1 [S]
sales_experience = pred1 [X]
secretary	= pred1 []
college	= pred1 [L]
secondary_school	= pred1 [O]
local_business_club	= pred1 [C]
subject	= pred1 [M,G,H]	-- marketing, engineering, history
job	= pred1 [J]
raise	= pred1 [AR]
ideas	= pred1 [I]
story	= pred1 [Y]
thirty	= pred1 [B]
fifty_two	= pred1 [T]
forty_two	= pred1 [E]
successful	= pred1 [B,T,E]
personality	= pred1 [N]
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

good	= pred1 [AG]
bad	= pred1 [AB]

team_member	= pred1 [W]
co_worker	= pred1 [W,E,B]
co_workers	= [(E,B)]
possessions	= []
appreciation	= []
supervision	= [(T,Unspec)]
-- appearances	= [(B,Strong)]
assistance	= [(Unspec,T),(E,Unspec)]
volunteering	= [(E,C)]
teamplay	= [(B,AB),(T,AG),(E,AG)]


--(recruiter(boss,interviewer),site(company),worker,job)
recruitment	= [(F,B,R),(F,T,P),(F,E,R),(F,D,S)]
comms	= [ (B,Y,Unspec),(A,Y,Unspec),(E,Y,Unspec) ]
giving	= [(W,I,T)]

-- (teacher,school(location),subject,student)
schooling = [(Unspec,O,M,B),(Unspec,L,G,T),(Unspec,L,H,E)]

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
interviewer = pred1 $ map agent recruitment
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
		++ co_workers ++ map swap co_workers
		++ ( map (\x->(x,N) ) $ filter people entities ) -- personality

data Want a where
	Thing :: Entity -> Want Entity
	Promo :: Entity -> Want Entity
	Event :: (Entity,Entity,Entity) -> Want Entity
wants :: [(Entity,Want Entity)]
wants	= [(B,Thing AR),(B,Promo S),(T,Promo S),(E,Promo S)]

becoming	= pred2 [(B,S),(T,S),(E,S)]
knowledge	= []
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
appreciate	= pred2 appreciation
help	= pred2 assistance
volunteer	= pred1 $ map (\x -> fst x ) volunteering
volunteer_at	= pred2 volunteering
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
