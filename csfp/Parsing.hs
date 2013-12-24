module Parsing where

import Data.List
import Data.Char

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = "\n\tLeaf " ++ "(" ++ show t ++ ")"
  show (Branch l ts) = "\nBranch " ++ "\t" ++ show l  ++ "\t" 
                            ++ show ts ++ "\n"
type Pos = [Int]

pos ::  ParseTree a b -> [Pos]
pos Ep            = [[]]
pos (Leaf _)      = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, 
                                     p <- pos t ]

subtree :: ParseTree a b -> Pos -> ParseTree a b 
subtree t             []     = t
subtree l@(Leaf _)    _      = l
subtree (Branch _ ts) (i:is) = subtree (ts!!i) is 

subtrees :: ParseTree a b -> [ParseTree a b]
subtrees t = [ subtree t p | p <- pos t ]

get_phon :: CatLabel -> ParseTree Cat Cat -> Phon
get_phon label tree = get ps tree
	where
	    ps = pos tree
	    get [] _		= "No label/phon"
	    get [p] t           = phon (t2c(subtree t p))
	    get (p:ps') t	| (catLabel (t2c(subtree t p))) == label
				    = phon (t2c(subtree t p))
				| otherwise = get ps' t
	

isNg :: ParseTree Cat Cat -> Bool
isNg tree | catLabel (t2c tree) == "S" =
    (elem Ng . fs. t2c . flip subtree [0,1,0,1]) tree
isNg tree = error (show tree ++ "tree not a top level tree")

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat
          | Pers  | Refl | Wh 
          | Tense | Infl | Part
          | Act   | Pass
          | Pos   | Ng
          | About | After | Around | At | As | BecauseOf
          | In | Like | On | For | With
          | By | To | ToInf | From | Through
          | Of
          deriving (Eq,Show,Ord)

type Agreement = [Feat]

gender, number, person, gcase, pronType, tense, prepType, advType, posType, polarity
		 :: Agreement -> Agreement
gender   = filter (`elem` [MascOrFem,Masc,Fem,Neutr])
number   = filter (`elem` [Sg,Pl])
person   = filter (`elem` [Fst,Snd,Thrd])
gcase    = filter (`elem` [Nom,AccOrDat])
pronType = filter (`elem` [Pers,Refl,Wh]) 
tense    = filter (`elem` [Tense,Infl,Part]) 
voice    = filter (`elem` [Act,Pass])
prepType = filter (`elem` [About,After,As,At,BecauseOf,Like,In,On,For,With,By,To,From,Through]) 
advType = filter (`elem` [Around,In,On]) 
posType  = filter (`elem` [Of])
polarity  = filter (`elem` [Pos,Ng])

prune :: Agreement -> Agreement
prune fs = if   (Masc `elem` fs || Fem `elem` fs)
           then (delete MascOrFem fs) 
           else fs 

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = "Cat " ++ label ++ show agr ++ show subcatlist
  show (Cat phon label agr subcats) = "Cat " ++ phon  ++ " " ++ label ++ show agr ++ show subcats

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement 
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 = 
 [ feats | length (gender   feats) <= 1, 
           length (number   feats) <= 1, 
           length (person   feats) <= 1, 
           length (gcase    feats) <= 1,
           length (pronType feats) <= 1,
           length (tense    feats) <= 1,
           length (prepType feats) <= 1,
           length (polarity feats) <= 1
	   ]
  where 
    feats = (prune . nub . sort) (fs cat1 ++ fs cat2)

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not (null (combine cat1 cat2))

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = 
  [Cat phon label fs' subcatlist | 
         fs' <- combine c (Cat "" "" [f] []) ]

toupper = reverse . upperizer . reverse where
        upperizer (x:[]) = toUpper x : []
	upperizer (x:'_':xs) = (toUpper x) : '_': (upperizer xs)
	upperizer (x:xs) = x : upperizer xs

type Lexset = [ [Cat] ]

scan :: String -> String
scan []                      = []
scan ('\'':'s':xs)           = " 's" ++ scan xs
scan ('s':'\'':xs)           = "s 's" ++ scan xs
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

type Words = [String]

lexer :: String -> Words 
lexer = preproc . words . (map toLower) . scan



preproc :: Words -> Words
preproc []                 = []
preproc ["."]              = []
preproc ["?"]              = []


preproc ("6":",000":"dollars":xs)	= "6_000_dollars" : preproc xs
preproc ("3":",000":"dollars":xs)	= "3_000_dollars" : preproc xs
preproc ("the":"song,":"memory":xs)	= "the_song,_memory" : preproc xs
preproc ("the":"tv":"program,":"discovery":xs)	= "the_tv_program,_discovery" : preproc xs


preproc (",":xs)           = preproc xs

preproc ("applied":"foreign":"languages":xs)	= "applied_foreign_languages" : preproc xs
preproc ("minghsin":"university":xs)	= "minghsin_university" : preproc xs
preproc ("hello":"kitty":xs)	= "hello_kitty" : preproc xs
preproc ("avril":"lavigne":xs)	= "avril_lavigne" : preproc xs
preproc ("the":"color":"pink":xs)	= "the_color_pink" : preproc xs
preproc ("mi":"mi":xs)	= "mi_mi" : preproc xs
preproc ("brothers":"and":"sisters":xs)	= "brothers_and_sisters" : preproc xs
preproc ("april":"30th,":"1994":xs)	= "april_30th,_1994" : preproc xs
preproc ("truck":"driver":xs)	= "truck_driver" : preproc xs
preproc ("career":"woman":xs)	= "career_woman" : preproc xs
preproc ("listening":"to":"music":xs)	= "listening_to_music" : preproc xs
preproc ("design":"assistant":xs)	= "design_assistant" : preproc xs
preproc ("playing":"the":"piano":xs)	= "playing_the_piano" : preproc xs
preproc ("watching":"tv":xs)	= "watching_tv" : preproc xs
preproc ("going":"to":"the":"movies":xs)	= "going_to_the_movies" : preproc xs
preproc ("japanese":"interpreter":xs)	= "japanese_interpreter" : preproc xs
preproc ("non-commissioned":"officer":xs)	= "non-commissioned_officer" : preproc xs
preproc ("jeremy":"lin":xs)	= "jeremy_lin" : preproc xs
preproc ("hsiao":"ching-teng":xs)	= "hsiao_ching-teng" : preproc xs
preproc ("the":"military":xs)	= "the_military" : preproc xs
preproc ("making":"friends":xs)	= "making_friends" : preproc xs

preproc ("steve":"wynn":xs)	= "steve_wynn" : preproc xs
preproc ("entrance":"fee":xs)	= "entrance_fee" : preproc xs
preproc ("the":"ferrari":"showroom":xs)	= "the_ferrari_showroom" : preproc xs
preproc ("ten":"dollars":xs)	= "ten_dollars" : preproc xs

preproc ("punjabi":"farmers":xs)	= "punjabi_farmers" : preproc xs
preproc ("the":"punjabi":"government":xs)	= "the_punjabi_government" : preproc xs
preproc ("good":"idea":xs)	= "good_idea" : preproc xs
preproc ("citrus":"fruit":xs)	= "citrus_fruit" : preproc xs
preproc ("a":"good":"price":xs)	= "a_good_price" : preproc xs

preproc ("alex":"tew":xs)	= "alex_tew" : preproc xs
preproc ("the":"million":"dollar":"homepage":xs)	= "the_million_dollar_homepage" : preproc xs
preproc ("the":"one":"million":"people":"page":xs)	= "the_one_million_people_page" : preproc xs
preproc ("mark":"zuckerberg":xs)	= "mark_zuckerberg" : preproc xs
preproc ("set":"up":xs)	= "set_up" : preproc xs
preproc ("advertising":"space":xs)	= "advertising_space" : preproc xs
preproc ("decide":"to":"make":xs)	= "make" : preproc xs
preproc ("decided":"to":"make":xs)	= "made" : preproc xs
preproc ("radio":"and":"television":xs)	= "radio_and_television" : preproc xs
preproc ("business":"management":xs)	= "business_management" : preproc xs

preproc ("one":"month":xs)	= "one_month" : preproc xs
preproc ("two":"months":xs)	= "two_months" : preproc xs
preproc ("50":"dollars":"an":"hour":xs)	= "50_dollars_an_hour" : preproc xs
preproc ("a":"lot":"of":xs)	= "a_lot_of" : preproc xs
preproc ("lots":"of":xs)	= "lots_of" : preproc xs
preproc ("website":"designer":xs)	= "website_designer" : preproc xs

preproc("what's":xs)          = "what" : "was" : preproc xs
preproc("what're":xs)          = "what" : "were" : preproc xs

preproc ("grow":"up":xs)	= "grow_up" : preproc xs
preproc ("grew":"up":xs)	= "grew_up" : preproc xs
preproc ("take":"care":"of":xs)	= "take_care_of" : preproc xs
preproc ("took":"care":"of":xs)	= "took_care_of" : preproc xs
preproc ("looked":"after":xs)	= "looked_after" : preproc xs
preproc ("look":"after":xs)	= "look_after" : preproc xs
preproc ("cared":"for":xs)	= "cared_for" : preproc xs
preproc ("care":"for":xs)	= "care_for" : preproc xs
preproc ("bowel":"movement":xs)	= "bowel_movement" : preproc xs
preproc ("bowel":"movements":xs)	= "bowel_movements" : preproc xs
preproc ("water":"sports":xs)	= "water_sports" : preproc xs
preproc ("surfing":"accident":xs)	= "surfing_accident" : preproc xs

preproc ("throat":"cancer":xs)	= "throat_cancer" : preproc xs
preproc ("shrinking":"violet":xs)	= "shrinking_violet" : preproc xs
-- preproc ("turn":"around":xs)	= "turn_around" : preproc xs
-- preproc ("turned":"around":xs)	= "turned_around" : preproc xs
preproc ("hands":"on":"hips":xs)	= "hands_on_hips" : preproc xs
preproc ("front":"desk":xs)	= "front_desk" : preproc xs
preproc ("movie":"star":xs)	= "movie_star" : preproc xs
preproc ("new":"york":xs)	= "new_york" : preproc xs
preproc ("south":"africa":xs)	= "south_africa" : preproc xs

preproc ("mr":"batchelor":xs)	= "mr_batchelor" : preproc xs
preproc ("mr":"payne":xs)	= "mr_payne" : preproc xs
preproc ("business":"law":xs)	= "business_law" : preproc xs
preproc ("rutgers":"university":xs)	= "rutgers_university" : preproc xs


preproc ("sales":"representative":xs)	= "sales_representative" : preproc xs
preproc ("fast":"track":xs)	= "fast-track" : preproc xs
preproc ("regional":"manager":xs)	= "regional_manager" : preproc xs
-- preproc ("sales":"manager":xs)	= "sales_manager" : preproc xs
preproc ("secondary":"school":xs)	= "secondary_school" : preproc xs
preproc ("college":"degree":xs)	= "college_degree" : preproc xs
preproc ("local":"business":"club":xs)	= "local_business_club" : preproc xs
preproc ("sales":"record":xs)	= "sales_record" : preproc xs
preproc ("sales":"experience":xs)	= "sales_experience" : preproc xs
preproc ("team":"member":xs)	= "team_member" : preproc xs
preproc ("team":"members":xs)	= "team_members" : preproc xs
preproc ("thirty":"years":"old":xs)	= "thirty_years_old" : preproc xs
preproc ("fifty-two":"years":"old":xs)	= "fifty-two_years_old" : preproc xs
preproc ("forty-two":"years":"old":xs)	= "forty-two_years_old" : preproc xs

preproc ("the":"united":"states":xs)	= "the_united_states" : preproc xs

preproc ("electrical":"engineering":xs)	= "electrical_engineering" : preproc xs
preproc ("mechanical":"engineering":xs)	= "mechanical_engineering" : preproc xs
preproc ("jensen":"huang":xs)	= "jensen_huang" : preproc xs
preproc ("morris":"chang":xs)	= "morris_chang" : preproc xs
preproc ("master":"'s":"degree":xs)	= "master's_degree" : preproc xs
preproc ("phd":"degree":xs)	= "phd_degree" : preproc xs

preproc ("stanford":"university":xs)	= "stanford_university" : preproc xs

preproc ("ingredients":"for":"success":xs)	= "ingredients_for_success" : preproc xs
preproc ("ingredient":"for":"success":xs)	= "ingredient_for_success" : preproc xs
preproc ("clear":"and":"simple":"idea":xs)	= "clear_and_simple_idea" : preproc xs
preproc ("compcomp":"activity":xs)	= "compcomp_activity" : preproc xs

preproc ("fruit":"store":xs)	= "fruit_store" : preproc xs
preproc ("shoe":"store":xs)	= "shoe_store" : preproc xs
preproc ("too":"far":xs)	= "too_far" : preproc xs
preproc ("jogging":"shoes":xs)	= "jogging_shoes" : preproc xs
preproc ("men":"'s":"formal":"shoes":xs)	= "men's_formal_shoes" : preproc xs
preproc ("women":"'s":"formal":"shoes":xs)	= "women's_formal_shoes" : preproc xs
preproc ("women":"'s_formal_shoes":xs)	= "women's_formal_shoes" : preproc xs

preproc ("500":"nt":"and":"up":xs)	= "500_nt_and_up" : preproc xs
preproc ("1":",200":"nt":"and":"up":xs)	= "1_200_nt_and_up" : preproc xs
preproc ("1":",000":"nt":"and":"up":xs)	= "1_000_nt_and_up" : preproc xs

preproc ("dr":"bean":xs)	= "dr_bean" : preproc xs
preproc ("steve":"fossett":xs)	= "steve_fossett" : preproc xs
preproc ("ellen":"macarthur":xs)	= "ellen_macarthur" : preproc xs
preproc ("powered":"aircraft":xs)	= "powered_aircraft" : preproc xs

preproc ("european":"campers":xs)	= "european_campers" : preproc xs
preproc ("charles":"holden":xs)	= "charles_holden" : preproc xs
preproc ("dot":"gourlay":xs)	= "dot_gourlay" : preproc xs

preproc ("office":"worker":xs)	= "office_worker" : preproc xs
preproc ("production":"manager":xs)	= "production_manager" : preproc xs
preproc ("sales":"manager":xs)	= "sales_manager" : preproc xs
preproc ("slow":"living":xs)	= "slow_living" : preproc xs
preproc ("lack":"of":"control":xs)	= "lack_of_control" : preproc xs
preproc ("lack":"of":"support":xs)	= "lack_of_support" : preproc xs
preproc ("put":"pressure":xs)	= "put_pressure" : preproc xs
preproc ("feel":"stress":xs)	= "feel_stress" : preproc xs
preproc ("felt":"stress":xs)	= "felt_stress" : preproc xs
preproc ("cause":"stress":xs)	= "cause_stress" : preproc xs
preproc ("caused":"stress":xs)	= "caused_stress" : preproc xs

preproc ("eagle":"scout":xs)	= "eagle_scout" : preproc xs
preproc ("troop":"409":xs)	= "troop_409" : preproc xs
preproc ("assistant":"scoutmaster":xs)	= "assistant_scoutmaster" : preproc xs
preproc ("look":"back":xs)	= "look_back" : preproc xs
preproc ("looked":"back":xs)	= "looked_back" : preproc xs
preproc ("got":"married":xs)	= "got_married" : preproc xs
preproc ("get":"married":xs)	= "get_married" : preproc xs
preproc ("mentally":"disabled":xs)	= "mentally-disabled" : preproc xs
preproc ("physically":"disabled":xs)	= "physically-disabled" : preproc xs
preproc ("traffic":"accident":xs)	= "traffic_accident" : preproc xs
preproc ("brain":"damage":xs)	= "brain_damage" : preproc xs

preproc ("the":"state":"of":"colorado":xs) = "the_state_of_colorado" : preproc xs
preproc ("the":"gathering":"place":xs)  = "the_gathering_place" : preproc xs
preproc ("ten":"dollar":"bill":xs)      = "ten_dollar_bill" : preproc xs
preproc ("administrative":"assistant":xs) = "administrative_assistant" : preproc xs
preproc ("birthday":"card":xs)  = "birthday_card" : preproc xs

preproc ("how":"much":xs)	= "how_much" : preproc xs

preproc ("because":"of":xs)	= "because_of" : preproc xs

preproc ("an":xs)	= "a" : preproc xs
preproc ("did":"not":xs)   = "didn't" : preproc xs
preproc ("nothing":xs)     = "no"    : "thing"  : preproc xs
preproc ("nobody":xs)      = "no"    : "person" : preproc xs
preproc ("no-one":xs)      = "no"    : "person" : preproc xs
preproc ("no":"one":xs)    = "no"    : "person" : preproc xs
preproc ("something":xs)   = "some"  : "thing"  : preproc xs
preproc ("somebody":xs)    = "some"  : "person" : preproc xs
preproc ("someone":xs)    = "some"  : "person" : preproc xs
preproc ("everything":xs)  = "every" : "thing"  : preproc xs
preproc ("everybody":xs)   = "every" : "person" : preproc xs
preproc ("everyone":xs)   = "every" : "person" : preproc xs
preproc ("less":"than":xs) = "less_than" : preproc xs
preproc ("more":"than":xs) = "more_than" : preproc xs
preproc ("at":"least":xs)  = "at_least"  : preproc xs
preproc ("at":"most":xs)   = "at_most"   : preproc xs
preproc (x:xs)             = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = db w

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words = 
  let
    listing = map (\ x -> (x,lookupWord db x)) words
    unknown = map fst (filter (\x -> snd x == unknownWord) listing)

  in
    if unknown /= [] then 
      error ("unknown words: " ++ show unknown)
    else initCats (map snd listing) 
	where unknownWord = [Cat "" "" [] []]

initCats :: [[Cat]] -> [[Cat]]
initCats []         = [[]]
initCats (cs:rests) = [ c:rest | c    <- cs, 
                                 rest <- initCats rests ]

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

agreeC :: ParseTree Cat Cat -> ParseTree Cat Cat -> Bool
agreeC t1 t2 = agree (t2c t1) (t2c t2) 

assignT :: Feat ->  ParseTree Cat Cat 
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]


match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y 
	      && agree x y 
	      && match xs ys 

balancefs :: ParseTree Cat Cat -> [ Feat ]
balancefs t = 
	let feats = fs $ t2c t
		in map polarize feats
		where polarize feat = case feat of
			Pos -> Ng
			Ng  -> Pos
			otherwise -> feat


type StackParser a b = [a] -> [a] -> [(b,[a],[a])]

type SPARSER a b = StackParser a (ParseTree a b)

infixr 4 <||>

(<||>) :: StackParser a b -> StackParser a b 
		  -> StackParser a b 
(p1 <||> p2) stack xs = p1 stack xs ++ p2 stack xs 

infixl 6 <::>

(<::>) :: StackParser a b  -> StackParser a [b] 
                           -> StackParser a [b]
(p <::> q) us xs = [(r:rs,ws,zs) | (r,vs,ys)  <- p us xs,
                                   (rs,ws,zs) <- q vs ys ]

succeedS :: b -> StackParser a b 
succeedS r us xs = [(r,us,xs)]

manyS :: StackParser a b -> StackParser a [b]
manyS p = (p <::> manyS p) <||> succeedS []

oneS :: [StackParser a b] -> StackParser a [b]
oneS (p:ps) = p <::> succeedS []

push :: Cat -> SPARSER Cat Cat -> SPARSER Cat Cat 
push c p stack = p (c:stack) 
pushlist :: [Cat] -> SPARSER Cat Cat -> SPARSER Cat Cat 
pushlist c p stack = p (c ++ stack)

pop :: CatLabel -> SPARSER Cat Cat 
pop c []     xs                   = []
pop c (u:us) xs | catLabel u == c = [(Leaf u, us, xs)]
                | otherwise       = []

leafPS :: CatLabel -> SPARSER Cat Cat
leafPS l _ []         = [] 
leafPS l s (c:cs) = [(Leaf c,s,cs) | catLabel c == l ]

prsTXT :: SPARSER Cat Cat
prsTXT = conjR <||> prsS

conjR :: SPARSER Cat Cat 
conjR = \ us xs -> 
   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], ws, zs) | 
       (s,vs,ys)      <- prsS us xs,
       (conj,vs1,ys1) <- leafPS "CONJ" vs ys, 
       (txt,ws,zs)    <- prsTXT vs1 ys1            ]

prsS :: SPARSER Cat Cat
prsS = spR <||> cond1R <||> cond2R

prsTAG :: SPARSER Cat Cat
prsTAG = \us xs -> [ (Branch (Cat "_" "S" [] []) [s,t],ws,zs) |
	(s,vs,ys) <- spTagR us xs,
	(t,ws,zs) <- tagR vs ys ]

tagR :: SPARSER Cat Cat 
tagR = \ us xs -> 
 [ (Branch (Cat "_" "TAG" fs []) [tagV,tagS],rs,ss) | 
	(tagV,vs,ys)	<- prsCOPAUX us xs,
	(aux,ws,zs)	<- popCOPAUX vs ys,
	agreeC tagV aux,
	subcatList (t2c tagV) == [],
	and $ zipWith (==) (phon (t2c tagV)) (phon (t2c aux)),
	(tagS,ps,qs)	<- leafPS "NP" ws zs,
	(subj,rs,ss)	<- pop "NP" ps qs,
	agreeC tagS subj,
	agreement	<- combine (t2c tagV) (t2c tagS),
	fs		<- [polarity agreement ++ person agreement ++
				number agreement ++ gender agreement]
	]

popCOPAUX :: SPARSER Cat Cat
popCOPAUX = pop "AUX" <||> pop "COP"

spTagR :: SPARSER Cat Cat 
spTagR = \ us xs -> 
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np',vp],ws,zs) | 
       (np,vs,ys) <- prsNP us xs,
       np'        <- assignT Nom np, 
       tag        <- [Cat (phon (t2c np')) (catLabel (t2c np')) (fs (t2c np')) [] ],
       (vp,ws,zs) <- prsVP (vs ++ [tag]) ys,
       agreeC np vp,
       subcatList (t2c vp) == []
       ]

spR :: SPARSER Cat Cat 
spR = \ us xs -> 
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np',vp],ws,zs) | 
       (np,vs,ys) <- prsNP us xs,
       (vp,ws,zs) <- prsVP vs ys, 
       np'        <- assignT Nom np, 
       agreeC np vp,
       subcatList (t2c vp) == [] ]

cond1R :: SPARSER Cat Cat 
cond1R = \ us xs -> 
   [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
       (cond,vs,ys) <- leafPS "COND" us xs, 
       (s1,vs1,ys1) <- prsS vs ys,
       (s2,ws,zs)   <- prsS vs1 ys1 ]

cond2R :: SPARSER Cat Cat 
cond2R = \ us xs -> 
     [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) | 
         (cond,vs,ys) <- leafPS "COND" us xs, 
         (s1,vs1,ys1) <- prsS vs ys,
         (_,vs2,ys2)  <- leafPS "THEN" vs1 ys1, 
         (s2,ws,zs)   <- prsS vs2 ys2 ]

infinR1 :: SPARSER Cat Cat
infinR1 = \us xs ->
 [ (Branch (Cat "_" "AT" [] []) [vp1,to,vp2],ps,qs) |
	(vp1,vs,ys)  <- leafPS "V" us xs, 
	(to,ws,zs) <- prsTO vs ys,
	(z:zs')    <- [zs],
	catLabel z == "V",
	z'         <- [Cat (phon z) "V" [Tense] (subcatList z)],
	(vp2,ps,qs) <- prsVP ws (z':zs') ]

infinR2 :: SPARSER Cat Cat 
infinR2 = \ us xs -> 
 [ (Branch (Cat "_" "AT" (fs (t2c vp1)) []) [vp1,obj,to,vp2],rs,ss) | 
	(vp1,vs,ys) <- leafPS "V" us xs, 
	(obj,ws,zs) <- prsNP vs ys,
	(to,ps,qs)  <- prsTO ws zs,
	(q:qs')     <- [qs],
	catLabel q == "V",
	q'          <- [Cat (phon q) "V" [Tense] (subcatList q)],
	(vp2,rs,ss) <- prsVP ps (q':qs') ]
	-- subcatlist   <- [subcatList (t2c att)],
	-- match subcatlist [t2c inf] ]

prsNP :: SPARSER Cat Cat 
prsNP = leafPS "NP" <||> npR <||> npADJR <||> npposR <||> cnposR <||> adjcnposR <||> depCR  <||> pop "NP" 

npR :: SPARSER Cat Cat
npR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,cn], (us++ws), zs) | 
      (det,vs,ys) <- prsDET [] xs,
      (cn,ws,zs)  <- prsCN vs ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn
      ]

npADJR :: SPARSER Cat Cat
npADJR = \ us xs -> 
  [ (Branch (Cat "_" "NP" fs []) [det,adj,cn], (us++ss), ts) | 
      (det,vs,ys) <- prsDET [] xs,
      (adj,ws,zs)  <- prsADJ vs ys,
      (cn,ss,ts)  <- prsCN ws zs,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]

npposR :: SPARSER Cat Cat
npposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [np,pos,cn], (us++ss), ts) |
      (np,vs,ys) <- leafPS "NP" [] xs,
      (pos,ws,zs) <- prsAPOS vs ys,
      (cn,ss,ts)  <- prsCN ws zs
      ]

cnposR :: SPARSER Cat Cat
cnposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [Branch (Cat "_" "NP" fs []) [det,cn1],pos,cn2], (us++qs), rs) |
      (det,vs,ys) <- prsDET [] xs,
      (cn1,ws,zs)  <- prsCN vs ys,
      fs         <- combine (t2c det) (t2c cn1),
      agreeC det cn1,
      (pos,ss,ts) <- prsAPOS ws zs,
      (cn2,qs,rs)  <- prsCN ss ts
      ]

adjcnposR :: SPARSER Cat Cat
adjcnposR = \us xs ->
  [ (Branch (Cat "_" "NP" [] []) [Branch (Cat "_" "NP" fs []) [det,adj,cn1],pos,cn2], (us++os), ps) |
      (det,vs,ys) <- prsDET [] xs,
      (adj,ws,zs)  <- prsADJ vs ys,
      (cn1,ss,ts)  <- prsCN ws zs,
      fs         <- combine (t2c det) (t2c cn1),
      agreeC det cn1,
      (pos,qs,rs) <- prsAPOS ss ts,
      (cn2,os,ps)  <- prsCN qs rs
      ]

depCR :: SPARSER Cat Cat
depCR = \us xs ->
  [ (Branch (Cat "_" "NP" (fs (t2c vp)) []) [ing,Branch (Cat "_" "VP" [] []) [vp,xps] ], (us++ss), ts) |
      (vp,vs,ys)  <- leafPS "V" us xs, -- TODO us? []?
      (ing,ws,zs)  <- prsGER vs ys,
      verb	<- [t2c vp],
      (xps,ss,ts)  <- vpR ws (verb:zs)
      ]

prsGER :: SPARSER Cat Cat
prsGER = leafPS "GER"

prsAPOS :: SPARSER Cat Cat
prsAPOS = leafPS "APOS"

prsOFPOS :: SPARSER Cat Cat
prsOFPOS = leafPS "OFPOS"

prsZERO :: SPARSER Cat Cat
prsZERO = succeedS $ Leaf (Cat "zero" "DET" [Pl] [])

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET" <||> prsZERO 

prsADJ :: SPARSER Cat Cat
prsADJ = leafPS "ADJ"

prsCN :: SPARSER Cat Cat
prsCN = leafPS "CN" <||> cnrelR <||> ofR

prsVP :: SPARSER Cat Cat
prsVP = finVpR <||> auxVpR <||> copR

copR :: SPARSER Cat Cat
copR = cop1R <||> cop2R

cop1R = \us xs -> [(Branch (Cat "_" "VP" (fs (t2c cop)) []) [cop,Branch (Cat "_" "COMP" [] []) [comp]],ws,zs) |
	(cop,vs,ys)  <- prsCOP us xs,
	tag          <- [Cat (phon (t2c cop)) (catLabel(t2c cop)) (balancefs cop) [] ],
	(comp,ws,zs) <- case us of
		[Cat _ "NP" _ _] -> push tag prsCOMP vs ys
		otherwise -> prsCOMP vs ys,
	subcatlist   <- [subcatList (t2c cop)],
	match subcatlist [t2c comp]
		]

cop2R = \us xs -> [(Branch (Cat "_" "VP" (fs (t2c cop)) []) [cop,Branch (Cat "_" "COMP" [] []) [comp1,comp2]],ps,qs) |
	(cop,vs,ys)  <- prsCOP us xs,
	tag          <- [Cat (phon (t2c cop)) (catLabel(t2c cop)) (balancefs cop) [] ],
	(comp1,ws,zs) <- case us of
		[Cat _ "NP" _ _] -> push tag prsCOMP vs ys
		otherwise -> prsCOMP vs ys,
	subcatlist   <- [subcatList (t2c cop)],
	match subcatlist [t2c comp1],
	(comp2,ps,qs)	<- prsCOMP ws zs
		]

prsCOP :: SPARSER Cat Cat
prsCOP = leafPS "COP" <||> pop "COP"

prsCOMP :: SPARSER Cat Cat
prsCOMP = prsNP <||> prsADJ <||> prsPP -- <||> nPandPPR

--nPandPPR :: SPARSER Cat Cat
--nPandPPR = \us xs ->
-- [(Branch (Cat "_" "COMP" [no]) [],ws,zs) |
--    (np,vs,ys) <- prsNP us xs,
--    (pp,ws,zs) <- prsPP vs ys ]

vpR :: SPARSER Cat Cat
vpR = vp0R <||> vp1R <||> vp2R <||> vp3R <||> infinR1 <||> infinR2

vp0R :: SPARSER Cat Cat
vp0R = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) [vp],vs,ys) |  
             (vp,vs,ys)  <- leafPS "V" us xs,
	     subcatList (t2c vp) == [] ]

vp1R :: SPARSER Cat Cat
vp1R = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) [vp,xp],ws,zs) |  
             (vp,vs,ys)  <- leafPS "V" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (xp,ws,zs) <- prsNPorPP vs ys, 
             match subcatlist [t2c xp] ]

vp2R :: SPARSER Cat Cat
vp2R = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) [vp,obj1,obj2],ps,qs) |  
             (vp,vs,ys)  <- leafPS "V" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (obj1,ws,zs) <- prsNPorPP vs ys, 
             (obj2,ps,qs) <- prsNPorPP ws zs, 
             match subcatlist (map t2c [obj1,obj2]) ]

vp3R :: SPARSER Cat Cat
vp3R = \us xs -> 
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) [vp,obj1,obj2,pp],rs,ss) |  
             (vp,vs,ys)  <- leafPS "V" us xs, 
             subcatlist  <- [subcatList (t2c vp)],
             (obj1,ws,zs) <- prsNPorPP vs ys, 
             (obj2,ps,qs) <- prsNPorPP ws zs, 
             (pp,rs,ss)   <- prsPP ps qs, 
             match subcatlist (map t2c [obj1,obj2,pp]) ]

finVpR :: SPARSER Cat Cat
finVpR = \us xs -> [(vp',vs,ys) | 
		tag        <- [Cat "didn't" "AUX" [ Ng ] [] ],
                (vp,vs,ys) <- case us of
			[Cat _ "NP" _ _] -> push tag vpR us xs
			otherwise -> vpR us xs,
		vp'        <- assignT Tense vp ]

auxVpR :: SPARSER Cat Cat
auxVpR = \us xs -> [ (Branch (Cat "_" "VP" (fs (t2c aux)) []) 
	[aux,inf'], ws, zs) | 
	(aux,vs,ys) <- prsAUX us xs,
	tag        <- [Cat (phon (t2c aux)) (catLabel(t2c aux)) (balancefs aux) []],
	(inf,ws,zs) <- case us of
		[Cat _ "NP" _ _]	-> push tag vpR vs ys
		otherwise	-> vpR vs ys,
	inf'       <- case (phon (t2c aux), fs (t2c inf) ) of
		("was",	[Part])	-> assignT Part inf
		("#was",    [Part])	-> assignT Part inf
		("wasn't",  [Part])	-> assignT Part inf
		("#wasn't", [Part])	-> assignT Part inf
		("were",    [Part])	-> assignT Part inf
		("#were",   [Part])	-> assignT Part inf
		("weren't", [Part])	-> assignT Part inf
		("#weren't",[Part])	-> assignT Part inf
		("did",	[Infl])	-> assignT Infl inf
		("#did",    [Infl])	-> assignT Infl inf
		("didn't",  [Infl])	-> assignT Infl inf
		("#didn't", [Infl])	-> assignT Infl inf
		otherwise   -> case catLabel (t2c inf) of
		    "AT" -> [inf]
		    otherwise -> []
		    
			]

prsAUX :: SPARSER Cat Cat
prsAUX = leafPS "AUX" <||> pop "AUX" 

prsPP :: SPARSER Cat Cat
prsPP = ppR <||> pop "PP" 

ppR :: SPARSER Cat Cat
ppR = \us xs -> 
  [ (Branch (Cat "_" "PP" fs []) [prep,np'], ws, zs) | 
      (prep,vs,ys) <- prsPREP us xs, 
      (np,ws,zs)   <- prsNP vs ys,
       np'         <- assignT AccOrDat np, 
       fs          <- combine (t2c prep) (t2c np') ]

prsPREP :: SPARSER Cat Cat
prsPREP = leafPS "PREP" <||> pop "PREP"

prsTO :: SPARSER Cat Cat
prsTO = leafPS "TO"

prsNPorPP :: SPARSER Cat Cat
prsNPorPP = prsNP <||> prsPP

prsCOMPorNPorPP :: SPARSER Cat Cat
prsCOMPorNPorPP = prsCOMP <||> prsNPorPP

prsVdependent :: SPARSER Cat Cat
prsVdependent = prsCOMPorNPorPP

prsVdependents :: [Cat] -> [Cat] 
      -> [([ParseTree Cat Cat],[Cat],[Cat])]
prsVdependents = manyS prsVdependent <||> manyS infinR1 <||> manyS infinR2

prsSorVdependents = prsVdependents <||> manyS prsS

cnrelR :: SPARSER Cat Cat
cnrelR = \us xs -> 
     [ (Branch (Cat "_" "RCN" (fs (t2c cn)) []) 
               [cn,rel], ws, zs) |
                 (cn,vs,ys)  <- leafPS "CN" us xs, 
                 (rel,ws,zs) <- prsREL vs ys, 
                 agreeC cn rel ]

ofR :: SPARSER Cat Cat
ofR = \us xs ->
  [ (Branch (Cat "_" "RCN" (fs (t2c cn1)) []) [cn1,pos,np2], (us++ss), ts) |
      (cn1,vs,ys)  <- leafPS "CN" us xs,
      (pos,ws,zs) <- prsOFPOS vs ys,
      (np2,ss,ts) <- prsNP ws zs
      ]

prsREL :: SPARSER Cat Cat 
prsREL = relclauseR <||> thatlessR <||> relppR

relclauseR :: SPARSER Cat Cat
relclauseR = \us xs -> 
  [(Branch (Cat "_" "MOD" fs []) [rel,s], ws, zs) |
      (rel,vs,ys) <- leafPS "REL" us xs, 
       fs         <- [fs (t2c rel)],
       gap        <- [Cat "#" "NP" fs []],
       (s,ws,zs)  <- push gap prsS vs ys ]

thatlessR :: SPARSER Cat Cat 
thatlessR = \ us xs -> 
        [ (Branch (Cat "_" "MOD" [] []) [s], vs, ys) | 
           gap       <- [Cat "#" "NP" [AccOrDat] []], 
           (s,vs,ys) <- push gap prsS us xs, 
           notElem Wh (fs (t2c s))                       ]

relppR :: SPARSER Cat Cat
relppR = \us xs -> 
     [ (Branch (Cat "_" "MOD" [] []) [pp], vs, ys) |
                 (pp,vs,ys) <- prsPP us xs ] 

prsYN :: SPARSER Cat Cat 
prsYN = \us xs -> 
   [(Branch (Cat "_" "YN" [] []) [dum,s], ws,zs) | 
       (dum,vs,ys) <- prsCOPAUX us xs, 
       gap         <- [Cat ("#"++phon (t2c dum) ) (catLabel (t2c dum)) (fs (t2c dum))
					       (subcatList (t2c dum)) ], 
       (s,ws,zs)   <- push gap prsS vs ys ]

prsCOPAUX :: SPARSER Cat Cat
prsCOPAUX = prsCOP <||> prsAUX

isWH :: ParseTree Cat Cat -> Bool
isWH tr = Wh `elem` (fs (t2c tr))

prsWH :: SPARSER Cat Cat 
prsWH = \us xs -> 
   [ (Branch (Cat "_" "WH" [] []) [wh,yn], ws,zs) | 
       (wh,vs,ys) <- prsCOMPorNPorPP us xs, 
       isWH wh, 
       gapfs      <- [filter (/= Wh) (fs (t2c wh))],
       gap        <- case (phon (t2c wh)) of 
			"when"	-> [[Cat "in" "PREP" [In] [], Cat "#" (catLabel (t2c wh)) gapfs []]]
			"where"	-> [[Cat "in" "PREP" [In] [], Cat "#" (catLabel (t2c wh)) gapfs []]]
			_	-> [[Cat "#" (catLabel (t2c wh)) gapfs []]],
       (yn,ws,zs) <- pushlist gap prsYNS vs ys ]

prsYNS :: SPARSER Cat Cat
prsYNS = prsYN <||> spR

testSuite1 :: [String]
testSuite1 = 
 [ 
   "Who left Rebia?",
   "Frank gave Rebia the ring.",
   "Did Frank give the ring to Rebia?",
   "Who did Frank give the rings to?",
   "To whom did Frank give the rings?",
   "Frank left the woman " ++ "that he gave the rings to.",
   "Who killed the man that helped the woman " 
    ++ "that had a boyfriend." ]

testSuite2 :: [String]
testSuite2 =  
 [ "Jack loved the woman that Frank helped Jill",
   "Rebia loved the man that helped"
    ]

-- vim: set ts=8 sts=4 sw=4 noet:
