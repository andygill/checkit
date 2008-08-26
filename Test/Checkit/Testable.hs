{-# LANGUAGE  UndecidableInstances, OverlappingInstances, FlexibleInstances, GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, ExistentialQuantification, FlexibleContexts #-}

module Test.Checkit.Testable where

import Control.Applicative
import System.Random
import Data.List
import Control.Monad

import Test.Checkit.ValueKey
import Test.Checkit.Interfaces
import Test.Checkit.Serial

import Prelude hiding (abs)

-------------------------------------------------------------------------------

instance (Property m prop) => TestableWith m prop Bool where
  property b = abs (return $! b)

instance (Serial a,TestableWith m prop b) => TestableWith m prop (a -> b) where
  property p = forAll p

-- may as well be first class in the DSL, because the type captures the arguments needed
forAll :: (TestableWith m p t, Serial a) => (a -> t) -> p (a -> t)
forAll f = abs' (\ rep -> getArgument (\ kv -> do
                             let opt_v = valueKeyToValue series kv
                             case opt_v of
                               (Just v) -> rep (property (f v))
                               Nothing  -> badKeyValue kv))

  where
    -- This hack is to avoid a a two way functional dependancy
    abs' :: (Property m p) => ((p a -> m Bool) -> m Bool) -> p b
    abs' f = abs (f rep)

-------------------------------------------------------------------------------

instance (Serial a,SerialArgs b) => SerialArgs (a -> b) where
  args = Bind series (args)

instance SerialArgs Bool where
  args = Nil


instance SerialArgs (IO ()) where
  args = Nil_IO

instance SerialArgs (IO Bool) where
  args = Nil_IO

-------------------------------------------------------------------------------


data TIMEOUT = TIMEOUT

--instance (TestableWith m prop b) => TestableWith m prop (TIMEOUT -> b) where
--  property p = error "opps"

------------------------------------------------------------------------------

data Measure a b = Measure a b

