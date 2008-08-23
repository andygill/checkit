{-# LANGUAGE  ExistentialQuantification, GADTs, TypeFamilies #-}
module Test.Checkit.Serial where

------------------------------------------------------------------------------

class Serial a where
  series :: Series a

data Series a = Data    [Cons a]
	      | NewType [Cons a]
	      | Prim (Integer -> a)	-- generating basic prims
                     (a -> String)      -- an a way of printing them

data Cons a = Cons String a
	    | forall b . Product (Cons (b -> a)) (Series b)


------------------------------------------------------------------------------

class SerialArgs a where
  args :: SeriesArgs a	-- the way to get arguments of a function


data SeriesArgs a where
  Bind :: (a ~ (b -> c)) => Series b     -> SeriesArgs c -> SeriesArgs a
  Prop :: (a ~ p b)      => SeriesArgs b -> SeriesArgs (p b)
  PAP  ::                   SeriesArgs a -> SeriesArgs a -- indicating that there is special significance to previous application
  Nil  :: 		    SeriesArgs a

showSeriesArgs :: SeriesArgs a -> String
showSeriesArgs (Bind _ c) = "* -> " ++ showSeriesArgs c
showSeriesArgs (Prop c)   = "P (" ++ showSeriesArgs c ++ ")"
showSeriesArgs (Nil)      = "@"

------------------------------------------------------------------------------

infixl 9 >< 

(><) :: Cons (a -> b) -> Series a -> Cons b
(><) p s = Product p s	-- moved the problem

cons0 :: String -> a -> Cons a
cons0 str a = Cons str a

cons1 n f = cons0 n f >< series
cons2 n f = cons1 n f >< series
cons3 n f = cons2 n f >< series

instance Serial () where
  series = Data -- "()"
		[ cons0 "()" ()
                ]


instance (Serial a,Serial b) => Serial (a,b) where
  series = Data -- "(,)"
		[ cons2 "(,)" (,)
		]

instance Serial Bool where
  series = Data -- "Bool"
		[ cons0 "True" True
		, cons0 "False" False
		]

instance Serial a => Serial [a] where
  series = Data -- "[]"
		[ cons2 "(:)" (:) 
		, cons2 "(:)" (:)       
--		, cons2 "(++)" (++)	-- nice hack for construct-like-combinators
		, cons0 "[]" []
		]	


frequency :: [(Int,a)] -> [a]
frequency = undefined

instance Serial Int where
  series = Prim (\ i -> fromInteger (toSigned i)) show

-- more generally, split in 2 dimensions
toSigned :: Integer -> Integer
toSigned n | even n    = n `div` 2
           | otherwise = -toSigned (n + 1)
                        

{-
-- need to work on Int
instance Serial Int where
  series = Data -- "Bool"
		[ cons "True" True
		, cons "False" False
		]

-- Utils 
boolsToNum :: [Bool] -> Integer
boolsToNum (True:xs)  = -x
  where  x = boolsToNum' (True:xs)
boolsToNum (False:xs) =  boolsToNum' (True:xs)
boolsToNum [] = 0

boolsToNum' :: [Bool] -> Integer
boolsToNum' (x:xs) = (2 ^ length xs * (if x then 1  else 0)) + boolsToNum' xs
boolsToNum' [] = 0
-}

------------------------------------------------------------------------------


------------------------------------------------------------------------------



