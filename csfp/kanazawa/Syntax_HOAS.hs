module Syntax_HOAS where

import Data.List

----------------
-- From FSynF.hs
----------------

data Sent = Sent NP VP deriving Show
data NP   = SnowWhite  | Alice  | Dorothy | Goldilocks 
          | LittleMook | Atreyu | Everyone | Someone
          | NP1 DET CN | NP2 DET RCN 
          deriving Show
data DET  = The | Every | Some | No | Most
          deriving Show
data CN   = Girl   | Boy   | Princess | Dwarf | Giant 
          | Wizard | Sword | Dagger
          deriving Show 
data ADJ  = Fake deriving Show
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          | RCN3 ADJ CN
          deriving Show
data That = That deriving Show
data VP   = Laughed | Cheered | Shuddered 
          | VP1 TV NP | VP2 DV NP NP
          | VP3 AV To INF
          deriving Show 
data TV   = Loved   | Admired | Helped 
          | Defeated | Caught
          deriving Show 

data DV   = Gave deriving Show
data AV   = Hoped | Wanted deriving Show 
data INF  = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch 
            deriving Show 
data To   = To deriving Show


------------------------------------------
-- Predicate Logic without bound variables
------------------------------------------

data Formula a = Atom String [Term a]         -- a is the type of parameter
               | Eq (Term a) (Term a)
               | Neg (Formula a)
               | Impl (Formula a) (Formula a)
               | Equi (Formula a) (Formula a)
               | Conj [Formula a]
               | Disj [Formula a] 
               | Forall (Term a -> Formula a)
               | Exists (Term a -> Formula a)
--               deriving Eq                    -- need an instance declaration for (Eq (Term -> Formula))

instance Show a => Show (Formula a) where
  show form = indexAndShow form 1 

indexAndShow :: Show a => Formula a -> Int -> String
indexAndShow (Atom s []) i   = s
indexAndShow (Atom s xs) i   = s ++ show xs
indexAndShow (Eq t1 t2) i    = show t1 ++ "==" ++ show t2
indexAndShow (Neg form) i    = '~' : (indexAndShow form i)
indexAndShow (Impl f1 f2) i  = "(" ++ indexAndShow f1 i ++ "==>" 
                         ++ indexAndShow f2 i ++ ")"
indexAndShow (Equi f1 f2) i  = "(" ++ indexAndShow f1 i ++ "<=>" 
                         ++ indexAndShow f2 i ++ ")"
indexAndShow (Conj []) i     = "true" 
-- indexAndShow (Conj fs) i     = "conj" ++ show (map (\f -> indexAndShow f i) fs)
indexAndShow (Conj fs) i     = "conj" ++ "[" ++ indexAndShowForms fs i ++ "]"
indexAndShow (Disj []) i     = "false" 
-- indexAndShow (Disj fs) i     = "disj" ++ show (map (\f -> indexAndShow f i) fs)
indexAndShow (Disj fs) i     = "disj" ++ "[" ++ indexAndShowForms fs i ++ "]"
indexAndShow (Forall scope) i  = "A x" ++ show i ++ (' ' : (indexAndShow (scope (Var i)) (i+1)))
indexAndShow (Exists scope) i  = "E x" ++ show i ++ (' ' : (indexAndShow (scope (Var i)) (i+1)))

indexAndShowForms :: Show a => [Formula a] -> Int -> String
indexAndShowForms [] _ = ""
indexAndShowForms [f] i = indexAndShow f i
indexAndShowForms (f:fs) i = indexAndShow f i ++ "," ++ indexAndShowForms fs i

data Term a = Param a | Var Int | Struct String [Term a] 
                  deriving Eq
--            deriving (Eq,Ord)

instance Show a => Show (Term a) where 
  show (Param param)     = show param 
  show (Var i)       = 'x' : show i
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

-- tx, ty, tz :: Term a
tx = Var 0
ty = Var 1
tz = Var 2


-- formula0, formula1, formula2 :: Formula a
formula0 = Atom "R" [tx,ty]
formula1 = Forall (\x -> (Atom "R" [x,x]))
formula2 = Forall (\x ->
            (Forall (\y ->
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))))
