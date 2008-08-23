{-# LANGUAGE GADTs, TypeFamilies #-}

module Test.Checkit.ValueKey where

import Data.List
import Control.Monad
import Test.Checkit.Serial			-- value keys decode serial
import System.Random

data ValueKey 
        = ValueKey Int [ValueKey]
        | PrimKey   Integer
	| BottomKey NamedBottom
	deriving Show

-- phantom type version of value key arguments to test candidate
newtype TypedValueKey s = TypedValueKey ValueKey
  deriving Show

-- phantom type version of value key arguments to test candidate
data ArgValueKeys s = ArgValueKeys [ValueKey]
  deriving Show

-- Perhaps should just be Exception, to be most general.
data NamedBottom = NamedBottom String
	deriving Show

newtype ValueKeys = ValueKeys { unValueKeys :: [ValueKey] }

--data ValueKeyBindees a where
--  Lam :: (a ~ (b -> c)) => b -> ValueKeyBindees c -> ValueKeyBindees a
--  Nil   :: ValueKeyBindees a

------------------------------------------------------------------------------

typedValueKeyToValue :: Series a -> TypedValueKey a -> Maybe a
typedValueKeyToValue s (TypedValueKey vk) = valueKeyToValue s vk

-- decode the value key, building a value
-- Perhaps this should use a failing version of the applicative monad
valueKeyToValue :: Series a -> ValueKey -> Maybe a
valueKeyToValue (Data alts) (BottomKey _) = return $ error "_|_"
valueKeyToValue (Prim fn _sh) (PrimKey n) = return $ fn n
valueKeyToValue (Data alts) (ValueKey ix keys) =
	if ix >= length alts 
	then fail "out of range alt selected"
	else valueKeyToValue' (alts !! ix) keys

valueKeyToValue' :: Cons a -> [ValueKey] -> Maybe a
valueKeyToValue' (Cons _ v)    []    = return v
valueKeyToValue' (Cons _ _)    (_:_) = fail "to many arguments"
valueKeyToValue' (Product _ _) []    = fail "to few arguments"
valueKeyToValue' (Product c s) keys  = valueKeyToValue' c (init keys) `ap` valueKeyToValue s (last keys)


showTypedValueKey :: Series a -> TypedValueKey a -> Maybe String
showTypedValueKey s (TypedValueKey ty) = showValueKey s ty


-- |@showValueKey@ takes a @Series@ (choice of shape) and @ValueKey@ a decoder for the choices, and returns
-- a value, lifted to allow for failure.
showValueKey :: Series a -> ValueKey -> Maybe String
showValueKey s v = showValueKey2 False s v

showValueKey2 :: Bool -> Series a -> ValueKey -> Maybe String
showValueKey2 b (Data alts) (BottomKey _) = return "_|_"
showValueKey2 b (Prim fn sh) (PrimKey n) = return (sh (fn n))
showValueKey2 b (Data alts) (ValueKey ix keys) =
	if ix >= length alts 
	then fail "out of range alt selected"
	else showValueKey' b (alts !! ix) keys

showValueKey' :: Bool -> Cons a -> [ValueKey] -> Maybe String
showValueKey' b ((Product (Product (Cons n v) s1) s2)) [k1,k2] 
	| head n == '(' && last n == ')' =
   liftM2 (\ a b -> "(" ++ a ++ tail (init n) ++ b ++ ")")
	 (showValueKey2 False s1 k1)	-- because application binds tigher than infix ops
	 (showValueKey2 False s2 k2)
					
showValueKey' b (Cons n v)    []    = return n
showValueKey' b (Cons _ _)    (_:_) = fail "to many arguments"
showValueKey' b (Product _ _) []    = fail "to few arguments"

showValueKey' True (Product c s) keys  = liftM (\ a -> "(" ++ a ++ ")") $ showValueKey' False (Product c s) keys 
showValueKey' b (Product c s) keys  = liftM2 (\ a b -> a ++ " " ++ b) 
					   (showValueKey' False c (init keys))
					   (showValueKey2 True s (last keys))

------------------------------------------------------------------------------

pickValueKey :: StdGen -> Series a -> ValueKey
pickValueKey stdGen (Data [])  = error "data with no alternatives"
pickValueKey stdGen (Prim fn _) = 
		 let (i,r) = randomR (0,100000) stdGen
                  in PrimKey i

pickValueKey stdGen (Data alts) = 
		 let (a,r) = randomR (0,length alts - 1) stdGen
		 in ValueKey a (consToValueKey r (alts !! a))
{-
			 let (a,r) = randomR (0,length alts) stdGen
			 in if a == length alts
			    then BottomKey (NamedBottom "ugg")
			    else ValueKey a (consToValueKey r (alts !! a))
-}
      where
	consToValueKey ::  StdGen -> Cons a -> [ValueKey]
	consToValueKey stdGen (Cons n _) = []
	consToValueKey stdGen (Product c s) = consToValueKey r1 c ++ [pickValueKey stdGen s]
	  where
		(r1,r2) = split stdGen


pickValueKeys :: StdGen -> SeriesArgs a -> ValueKeys
pickValueKeys stdGen sargs = ValueKeys $ pick stdGen sargs
  where       
  pick :: StdGen -> SeriesArgs a -> [ValueKey]
  pick stdGen (Bind s a) = pickValueKey r1 s : pick r2 a
    where (r1,r2) = split stdGen
  pick stdGen (Prop p)   = pick stdGen p
  pick stdGen Nil        = []

showArgValues :: SeriesArgs a -> ValueKeys -> Maybe String
showArgValues sa (ValueKeys vks) = fn sa vks
 where
   fn :: SeriesArgs a -> [ValueKey] -> Maybe String
   fn (Bind s a) (vk:vks) = do arg <- showValueKey s vk
                               rest <- fn a vks
                               return $ arg ++ " " ++ rest
                               
   fn (Prop p) vks = fn p vks
   fn (Nil)    []  = return "" 

