module Model where 

import Data.List

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities =  [minBound..maxBound] 

sean, beverly, marvin, mohamed, obama, shawn, annalisa, smoke
                                                :: Entity

sean = S
beverly = B
marvin = V
mohamed = M
obama = O
shawn = W
annalisa = L

smoke = K

type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

man, woman, dead, alive, hugger :: OnePlacePred

man     = list2OnePlacePred [S,W,M,V,O]
woman      = list2OnePlacePred [B,L]
dead      = list2OnePlacePred [S,B,M,V]
alive = list2OnePlacePred [O,L,W]
hugger = list2OnePlacePred [S]

person, thing :: OnePlacePred

person = \ x -> (man x || woman x)
thing  = \ x -> not (person x || x == Unspec)

thicken, whisper, talk, pause, die :: OnePlacePred

pause   = list2OnePlacePred [S]
talk   = list2OnePlacePred [S,B]
whisper = list2OnePlacePred [S]
die = list2OnePlacePred [S]
thicken = list2OnePlacePred [K]

love, admire, help, defeat :: TwoPlacePred

love   = curry (`elem` [(S,B)])
admire = curry (`elem` [(x,G) | x <- entities, person x])
help   = curry (`elem` [(W,W),(V,V),(S,B),(D,M)])
defeat = curry (`elem` [(x,y) | x <- entities, 
                                y <- entities,
                                man x && woman y]
                    ++ [(A,W),(A,V)])

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

give :: ThreePlacePred
give = curry3 (`elem` [(T,S,X),(A,E,S)])

kill :: ThreePlacePred
kill = curry3 (`elem` [(Y,T,F),(Unspec,D,X),
                       (Unspec,M,Unspec)])

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> r Unspec x

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 

