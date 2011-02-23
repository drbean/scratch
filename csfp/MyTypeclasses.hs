module MyTypeclasses where

import Prelude hiding ( Monad, Maybe, Nothing, Just, (>>=) )
--import Prelude hiding ( Text.Show.Functions )
-- import Control.Applicative hiding ( ZipList, getZipList )
import Data.Char

class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b ) -> f a -> f b
class Applicative m => Monad m where
	(>>=) :: m a -> ( a -> m b ) -> m b

data Maybe a = Nothing | Just a deriving Show
instance Functor Maybe where
	fmap _ Nothing = Nothing
	fmap g ( Just a ) = Just ( g a )
instance Applicative Maybe where
	pure = Just
	( Just g ) <*> ( Just a ) = Just ( g a )
instance Monad Maybe where 
	(>>=) ( Just x ) g = g x
	(>>=) Nothing _ = Nothing
--instance ( Show a ) => Show ( Maybe a ) where
--	show ( Just a )            = "Just" ++ " " ++ show a
--	show ( Nothing )      = "??"

data Wrapper a = Wrapper a deriving Show
instance Functor Wrapper where
	fmap g ( Wrapper x ) = Wrapper ( g x )
instance Applicative Wrapper where
	pure x = Wrapper x
	(<*>) (Wrapper g ) ( Wrapper x ) = Wrapper ( g x )
instance Monad Wrapper where
	(>>=) ( Wrapper x ) g = g x

newtype ZipList a = ZipList { getZipList :: [a] } deriving Show
instance Functor ZipList where
	fmap g ( ZipList as ) = ZipList ( fmap g as )
instance Applicative ZipList where
	pure a = ZipList [a]
	(<*>) ( ZipList gs ) ( ZipList xs ) = ZipList ( zipWith ($) gs xs )
instance Monad ZipList where
--	(>>=) ( ZipList as ) g = ZipList ( fmap g as )
	(>>=) ( ZipList [] ) _ = ZipList []
	(>>=) ( ZipList (x:xs) ) g = msum ( g x ) ( ( ZipList xs ) >>= g )
msum :: ZipList a -> ZipList a -> ZipList a
msum ( ZipList as ) ( ZipList bs ) = ZipList ( as ++ bs )
--instance ( Show a ) => Show ( ZipList a ) where
--	show ( ZipList [] ) = "ZipList "
--	show ( ZipList ( a:as ) ) = ( show a ) ++ ", " ++ show ( ZipList as )

instance Applicative [] where
	pure x = [x]
	gs <*> xs = [ g x | g <- gs, x <- xs ]
instance Monad [] where
	(>>=) [] _ = []
	(>>=) (x:xs) g = ( g x ) ++ ( (>>=) xs g )


--import Text.Show.Functions
--instance ( Show a,  Show b ) => Show ( (->) a b ) where
--	show  n _ = "Function" ++ show n
--instance ( Show a,  Show b ) => Show ( (->) a b ) where
--	show ( ($) a b ) = show a
--import Control.Functor.Pointed
--import Prelude hiding ( Maybe, Nothing, Just )

--instance Pointed Maybe where
--	-- point :: MyMaybe a => a -> MyMaybe a
--	point a   = Just a
--
--data MyMaybe a = Just a | Nothing
--
--instance Functor MyMaybe where
--	fmap _ MyTypeclasses.Nothing = MyTypeclasses.Nothing
--	fmap g ( MyTypeclasses.Just a ) = MyTypeclasses.Just ( g a )

---- data MyEither e a = Left e | Right a
--
----instance Functor MyEither e where
----	fmap _ MyTypeclasses.Left e = MyTypeclasses.Left e
----	fmap g ( MyTypeclasses.Right a ) = MyTypeclasses.Right ( g a )
--
---- instance ( Show e, Show a ) => Show ( MyEither e a ) where
----   show ( MyTypeclasses.Left e )            = show e
----   show ( MyTypeclasses.Right a )      = show a
--
--data MyEither e a = MyLeft e | MyRight a
--
--instance Functor ( MyEither e ) where
--	fmap _ ( MyLeft e ) = MyLeft e
--	fmap g ( MyRight a ) = MyRight ( g a )
--
--instance ( Show e, Show a ) => Show ( MyEither e a ) where
--  show ( MyLeft e )            = show e
--  show ( MyRight a )      = show a
--
--instance Functor ( Either e ) where
--	fmap _ ( Left e ) = Left e
--	fmap g ( Right a ) = Right ( g a )
--
---- instance ( Show e, Show a ) => Show ( MyEither e a ) where
----   show ( MyLeft e )            = show e
----   show ( MyRight a )      = show a
--
--instance Functor ( (,) e ) where
--	fmap g ( e, a ) = ( e, g a )
--
--instance Functor ( (->) e ) where
-- 	fmap ( g e ) _ = g e
--
--class Functor f => Pointed f where
--	pure :: a -> ( f a )
--
--instance Pointed ( Either e ) where 
--	pure = Right
