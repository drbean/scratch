module Mobit where 

import Data.List

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities =  [minBound..maxBound] 

sean, beverly, obama, catherine, annalisa, hillary
                                                :: Entity

sean = S
beverly = B
obama = O
annalisa = A
catherine = C
hillary = H
tragedy = T
advocate = V
lives = I
deaths = D
thanks = X
word = R
responders = P
friend = F
wtc = W
buff = U

type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

man, woman, dead, alive :: OnePlacePred

man     = list2OnePlacePred [S,O]
woman      = list2OnePlacePred [C,H,B,A]
dead      = list2OnePlacePred [S,B]

person, thing :: OnePlacePred

person = \ x -> (man x || woman x)
thing  = \ x -> not (person x || x == Unspec)
alive = \x -> ( person x && not ( dead x ) )

take_place, change, go_out, stand_out, arrive :: OnePlacePred

happen = list2OnePlacePred [T]
take_place   = list2OnePlacePred [T]
change   = list2OnePlacePred [I]
go_out = list2OnePlacePred [X]
stand_out = list2OnePlacePred [D]
arrive = list2OnePlacePred [R]

say, meet, lose ::  TwoPlacePred

say   = curry (`elem` [(O,R)])
meet = curry (`elem` [(O,B)])
lose   = curry (`elem` [(B,S)])
become = curry (`elem` [(B,V),(B,F)])
mark = curry (`elem` [(B,D)])
hit = curry (`elem` [(Unspec, wtc)])
go = curry ( `elem` [(B,buff)] )

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> or ( map ( flip  r x ) entities )

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 

--evaluation in model

type Interp a = String -> [a] -> Bool

int0 :: Interp Entity
int0 "Meet" = \ [x,y] -> meet x y
int0 "Lose" = \ [x,y] -> lose x y
int0 "Become" = \ [x,y] -> become x y
int0 "Mark" = \ [x,y] -> mark x y
int0 "Hit" = \ [x,y] -> hit x y
int0 "Go" = \ [x,y] -> go x y

int0 "Happen" = \ [x] -> happen x
int0 "Take_place" = \ [x] -> take_place x
int0 "Change" = \ [x] -> change x
int0 "Go_out" = \ [x] -> go_out x
int0 "Stand_out" = \ [x] -> stand_out x
int0 "Arrive" = \ [x] -> arrive x

