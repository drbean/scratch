module FSemF where 

import Data.List
import Data.Char (toUpper)
import FSynF
import System.Random

type Grid = [(Column,Row)]

exampleGrid :: Grid
exampleGrid = [(A',9),
               (B',4),(B',5),(B',6),(B',7),(B',9),
               (C',9),(D',4),(E',4),(F',4),
               (G',4),(G',7),(G',8),(G',9),
               (H',1),(H',4),(I',1)]

winningattack, attacks :: Grid
attacks = [(F',9),(E',8),(D',7),(C',6)]
winningattack = [(E',4),(F',4),(G',4),(H',4), -- needs (D',4) to win
		(B',5),(B',6),(B',7)]        -- needs (B'4) to win
battleship, frigate, sub1, sub2, destroyer :: Grid
battleship = [(D',4),(E',4),(F',4),(G',4),(H',4)]
frigate    = [(B',4),(B',5),(B',6),(B',7)]
sub1       = [(A',9),(B',9),(C',9)]
sub2       = [(G',7),(G',8),(G',9)]
destroyer  = [(H',1),(I',1)]

type State = ([Grid],Grid) 

shipsDistrib :: [Grid]
shipsDistrib = [battleship,frigate,sub1,sub2,destroyer]

exampleState = (shipsDistrib,attacks)
winningState = (shipsDistrib,winningattack)

noClashes :: State -> Bool
noClashes (distrib,_) = nodups (concat distrib) 
  where nodups []     = True
        nodups (x:xs) = notElem x xs && nodups xs

singleColumn :: Grid -> Bool
singleColumn ss = all ( \s -> fst( head ss ) == fst s ) ss
consecutiveRow :: Grid -> Bool
consecutiveRow ss = map snd ss == [ snd( head ss ) .. snd( last ss ) ]
singleRow :: Grid -> Bool
singleRow ss = all ( \s -> snd( head ss ) == snd s ) ss
consecutiveColumn :: Grid -> Bool
consecutiveColumn ss = ( map fst ss ) == [ fst( head ss ) .. fst( last ss ) ]
lineupOK :: State -> Bool
lineupOK( gs, _ ) = all ( \s -> singleColumn s && consecutiveRow s || singleRow s && consecutiveColumn s ) gs

hit :: Attack -> State -> Bool
hit pos (gs,_) = elem pos (concat gs)

missed :: Attack -> State -> Bool
missed pos = not . (hit pos)

defeated :: State -> Bool
defeated  (gs,g) = all (`elem` g) (concat gs)

shipShape :: Ship -> Int
shipShape Battleship = 5
shipShape Frigate    = 4
shipShape Submarine  = 3
shipShape Destroyer  = 2

sunk :: Attack -> Ship -> State -> Bool
sunk pos s (gs,aks)
   | missed pos (gs,aks) = False
   | elem pos aks        = False
   | notElem pos ( concat gs )    = False
   | any ( \ship -> ( all ( \g -> elem g (insert pos aks) ) ) ship ) ships = True
   | otherwise          = False
     where ships = filter (\g -> length g == (shipShape s)) gs

updateBattle :: Attack -> State -> State
updateBattle p (gs,g) = (gs, insert p g)

propNames :: Form -> [String]
propNames (P name) = [name]
propNames (Ng f)   = propNames f
propNames (Cnj fs) = (sort.nub.concat) (map propNames fs)
propNames (Dsj fs) = (sort.nub.concat) (map propNames fs)

genVals :: [String] -> [[(String,Bool)]]
genVals [] = [[]]
genVals (name:names) = map ((name,True) :) (genVals names) 
                    ++ map ((name,False):) (genVals names)

allVals :: Form -> [[(String,Bool)]]
allVals = genVals . propNames 

eval :: [(String,Bool)] -> Form -> Bool
eval [] (P c)    = error ("no info about " ++ show c)
eval ((i,b):xs) (P c) 
     | c == i    = b 
     | otherwise = eval xs (P c) 

eval xs (Ng f)   = not (eval xs f)
eval xs (Cnj fs) = all (eval xs) fs
eval xs (Dsj fs) = any (eval xs) fs

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies f1 f2 = contradiction (Cnj [f1,Ng f2])

impliesL :: [Form] -> Form -> Bool
impliesL f1 f2 = contradiction ( Cnj [Cnj f1, Ng f2] )

propEquiv :: Form -> Form -> Bool
propEquiv f1 f2
-- all ( \v -> eval v f2 ) (allVals f1) &&
-- all ( \v -> eval v f1 ) (allVals f2)
  | implies f1 f2 && implies f2 f1 = True
  | otherwise = False

update :: [[(String,Bool)]] -> Form -> [[(String,Bool)]]
update vals f = [ v | v <- vals, eval v f ]

genValStrings :: [String] -> [[String]]
genValStrings [] = [[]]
genValStrings (name:names) = map (name :) (genValStrings names) 
                    ++ (genValStrings names)

allValStrings :: Form -> [[String]]
allValStrings = genValStrings . propNames 

evalStrings :: [String] -> Form -> Bool
-- evalStrings [] (P propname)    = error ("no info about " ++ show propname)
evalStrings [] (P propname)    = False
evalStrings (p:ps) (P propname) 
     | p == propname    = True 
     | otherwise = evalStrings ps (P propname) 
evalStrings xs (Ng f)   = not (evalStrings xs f)
evalStrings xs (Cnj fs) = all (evalStrings xs) fs
evalStrings xs (Dsj fs) = any (evalStrings xs) fs


samepos :: Pattern -> Pattern -> Int
samepos _      []                 = 0 
samepos []     _                  = 0 
samepos (x:xs) (y:ys) | x == y    = samepos xs ys + 1
                      | otherwise = samepos xs ys 

occurscount ::  Pattern -> Pattern -> Int
occurscount xs []       = 0
occurscount xs (y:ys) 
          | y `elem` xs = occurscount (delete y xs) ys + 1
          | otherwise   = occurscount xs ys 

reaction :: Pattern -> Pattern -> [Answer]
reaction secret guess = take n (repeat Black) 
                     ++ take m (repeat White)
    where n = samepos secret guess 
          m = occurscount secret guess - n 

-- secret = [Red,Blue,Green,Yellow]

updateMM :: [Pattern] -> Pattern -> Feedback -> [Pattern]
updateMM state guess answer = 
   [ xs | xs <- state, reaction xs guess == answer ]

string2pattern :: String -> Pattern 
string2pattern = convertP . (map toUpper)

convertP :: String -> Pattern
convertP []       = []
convertP (' ':xs) =          convertP xs
convertP ('R':xs) = Red    : convertP xs
convertP ('Y':xs) = Yellow : convertP xs
convertP ('B':xs) = Blue   : convertP xs
convertP ('G':xs) = Green  : convertP xs
convertP ('O':xs) = Orange : convertP xs

playMM :: IO ()
playMM = 
  do
    secret <- getColours
    state <- return possibles
    loopMM secret state

loopMM :: Pattern -> [Pattern] -> IO ()
loopMM secret state = 
  do 
    putStrLn "Give a sequence of four colours from RGBYO"
    s <- getLine 
    guess <- return( string2pattern s )
    if guess /= secret 
      then 
        do 
          answer <- return( reaction secret (string2pattern s) )
	  newstate <- return( updateMM state guess answer )
          putStrLn (show answer)
	  if state == newstate
	    then
	      do putStrLn "That possibility had already been ruled out."
	   else putStrLn "Keep going."
          putStrLn "Please make another guess"
          loopMM secret newstate
      else putStrLn "correct"

getColours :: IO Pattern
getColours = do
              i <- getStdRandom (randomR (0,4))
              j <- getStdRandom (randomR (0,4))
              k <- getStdRandom (randomR (0,4))
              l <- getStdRandom (randomR (0,4))
              return [toEnum i,toEnum j, toEnum k, toEnum l]

possibles :: [Pattern]
possibles = [ [i, j, k, l] | i <- map toEnum [0..4] , j <- map toEnum [0..4] , k <- map toEnum [0..4] , l <- map toEnum [0..4]  ]
