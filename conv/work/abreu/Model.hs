module Model where 

import Data.Tuple
import Data.List
import Data.Maybe

data Entity	= A | B | C | C1 | C2 | D | E | F | G 
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | W1 | W2 | W3 | W4 | W5 | W6 | X | Y | Z | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 


--alice, rex, kelley, judy, rock
--                                                :: Entity

characters :: [ (String, Entity) ]

characters = [
	( "english",	E ),
	( "spanish",	H ),
	( "the_dominican_republic",	R ),
	( "the_united_states",	U ),
	( "boston_university",	V ),
	( "joan",	J ),
	( "john_doe",	D ),
	( "adela",	A ),
	( "claritza",	C )

	]

child	= pred1 [A]

boss = pred1 [J]
company = pred1 [O]
receptionist	= pred1 [C]
customer	= pred1 [D]
visitor	= customer
i_t	= pred1 [I]
school	= pred1 [V]
hospital = pred1 [P]
money	= pred1 [M]
story	= pred1 [Y]
job	= pred1 [B]
name	= pred1 [N]
language = pred1 [E,H]

namelist = map fst characters

demanding	= pred1 [J]
scared	= pred1 [C]
helpful	= pred1 [A]

male, female :: OnePlacePred

male	= pred1 [D,F,G]
female	= pred1 [J,A,C]

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

people, things :: OnePlacePred

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
aunt	= pred1 $ map fst aunting
niece = pred1 $ map snd aunting

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

--(parent,child)
parenting	= [(F,A),(G,F),(G,C)]
aunting	= [ (C,A) ]
supervision	= [(J,C)]
marriages	= []
--(husband,wife,wedding_location)
weddings	= []
--(divorcer,divorced)
separations	= []
-- divorces	= []
--(boyfriend,girlfriend)
-- unmarried_couples	= []
--(contacter,contactee)
possessions	= [ (J,O),(J,M),(C,M),(D,M) ]
business_relns	= [ (O,D) ]
appreciation	= []

raised_by	= pred2 $ map swap parenting
parented	= pred2 parenting
marry_in	= pred3 $ weddings ++ map (\(x,y,z) -> (y,x,z) ) weddings
married		= forgetful marry_in
separated	= pred2 separations
wedded_in	= pred2 $ map (\x -> (agent x, location x) ) weddings ++
			map (\x -> (patient x, location x) ) weddings
isMarried	= pred1 $ map fst marriages ++ map snd marriages
parentMaybe :: Entity -> (Entity,Entity) -> Maybe Entity
parentMaybe child = \rel -> if child == snd rel then Just (fst rel) else Nothing
parents		= \child -> mapMaybe (parentMaybe child) parenting
isSiblings	= \a b -> (any . flip elem) (parents a) (parents b)
brother	= \x -> any ( \i -> isSiblings x i ) entities

disappointments = []
disappoint	= pred2 $ disappointments
resent	= pred2 $ map swap disappointments
have	= pred2 $ possessions ++ business_relns
		++ marriages ++ parenting ++ supervision ++ aunting
		++ ( map swap $ marriages ++ parenting ++ supervision ++ aunting )
		++ ( map (\x->(recipient x, theme x) ) giving )
		++ ( map (\x->(agent x,B) ) working )
		++ ( map (\x->(snd x,N) ) characters )
knowledge	= [(C,E),(A,E),(J,E),(D,E),(F,E),(G,E),(C,H),(A,H),(F,H),(G,H)]
acquaintances	= []
know	= pred2 $ knowledge ++ acquaintances ++ map swap acquaintances
spelling	= [(J,N),(D,N),(A,N)]
spell	= pred2 spelling
speak	= \x y -> language y && know x y
appreciate	= pred2 appreciation
greet	= \x y -> receptionist x && visitor y
visit = pred2 $ map swap business_relns

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
working	= [(C,Unspec,O),(J,B,O),(D,B,P)]
comms	= [ (C,Y,Unspec),(A,N,C),(C,Unspec,D),(J,Unspec,C),(J,Unspec,D) ]
giving	= [ (J,B,C) ]
-- (seller, item, buyer)
selling	= []
--(killer,killed,instrument)
--(putter,theme,location)
--(agent,theme,location)
looking_back	= [(C,Unspec,Unspec)]
seeing	= []
--(agent,origin,destination)
immigration	= [(C,R,U)]

isImmigrant	= pred1 $ map agent immigration
worker	= pred1 $ map agent working
work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
look_back	= pred1 $ map agent looking_back
look_back_on	= pred2 $ map (\x->(agent x, theme x) ) looking_back
said	= pred2 $ map (\x->(agent x, theme x) ) comms
asked	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talked	= pred2 $ map (\x->(agent x, recipient x) ) comms
              ++  map (\(agent,theme,recipient)->(recipient, agent) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
come_from	= pred2 $ map (\x->(agent x, origin x) ) immigration
go_to	= pred2 $ map (\x->(agent x, destination x) ) immigration ++
			map (\x->(recipient4 x,location4 x) ) schooling
immigrate	= pred3 immigration


gave	= pred3 giving
got_from    = pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving
got = forgetful got_from
sold	= pred2 $ map (\x -> (agent x, theme x) ) selling

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r

-- (teacher,school(location),subject,student)
schooling = [(Unspec,V,I,C),(Unspec,R,I,C)]
studied = pred3 $ map ( \x -> (recipient4 x, theme4 x, location4 x) )
				schooling
studied_what = pred2 $ map (\x -> (recipient4 x, theme4 x) ) schooling
studied_where = pred2 $ map (\x -> (recipient4 x, location4 x) ) schooling
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
