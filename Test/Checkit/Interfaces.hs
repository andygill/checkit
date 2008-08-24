{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances, TypeFamilies,FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies  #-}

module Test.Checkit.Interfaces where

import Prelude hiding (abs)
import Control.Monad.Trans
import Test.Checkit.ValueKey
import Test.Checkit.Serial
import Control.Monad.Error.Class
import Control.Monad.Error

import System.Random hiding (getStdGen)

class Sum one all where
  inj :: one -> all
  sel :: all -> Maybe one

instance Sum a a where
  inj = id
  sel = Just

class Product mem whole where
  proj :: whole -> mem
  upd  :: mem -> whole -> whole

instance Product a a where
  proj a  = a	-- I
  upd e _ = e	-- K

------------------------------------------------------------------------------

class (Monad m) => RandomMonad m where
  getStdGen :: m StdGen

------------------------------------------------------------------------------

-- The inner context has less arguments in the argument stack 
class Monad m => Applicator m where
  getArgument    :: (ValueKey -> m a) -> m a
  applyArguments :: ValueKeys -> m a -> m a		-- 
  badKeyValue    :: ValueKey -> m a                     -- version of fail

------------------------------------------------------------------------------

-- A property without a genererator does not make much sence.
class (Applicator m) => Property m p | p -> m where
  rep :: p a -> m Bool
  abs :: m Bool -> p a

------------------------------------------------------------------------------

class (Property m prop,SerialArgs t) => TestableWith m prop t | prop -> m where
  -- "run" this test, and give me a property
  property :: t -> prop t


------------------------------------------------------------------------------

newtype AuditEvent msg = AuditEvent { unAuditEvent :: msg -> IO () }

class Monad m => Auditor chan m where
  report :: chan -> m ()	-- send a typed message to the audit sub-system

------------------------------------------------------------------------------

data LabelMsg = Label String			-- remember a label
     	      | ShowLabels			-- show a label

--label :: (Sum Label chan,Auditor chan m) => String -> m ()
--label :: (Sum Label all, Auditor all m) => String -> m ()
--label lab = report (inj (Label lab))


------------------------------------------------------------------------------

class (MonadIO m) => MonadFork m where
      forkM :: m () -> m (m ())	-- return a way to kill the child

------------------------------------------------------------------------------
