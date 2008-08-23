{-# LANGUAGE TypeSynonymInstances, 
             MultiParamTypeClasses, 
             UndecidableInstances, 
             FlexibleContexts, 
             ExistentialQuantification,

             FlexibleInstances, TypeOperators, FunctionalDependencies  #-}

module Test.Checkit.Check where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import qualified System.Random as R
import Test.Checkit.Utils as U

import qualified Control.Arrow as Arrow

import Test.Checkit.Testable
--import Test.Checkit.Applicator as App
import Test.Checkit.ValueKey
import Test.Checkit.Serial
import Test.Checkit.Interfaces
import Test.Checkit.TestMonad
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO

import Prelude hiding (abs)

------------------------------------------------------------------------------

type PM = TestM PMEnv PMState PMExc

newtype PMState = PMState { rnd :: R.StdGen }

instance Product R.StdGen PMState where
  proj (PMState v) = v
  upd v (PMState _) = PMState v

data    PMEnv   = PMEnv { applyMsg :: AuditEvent ApplyMsg
                        , labelMsg :: AuditEvent LabelMsg
			, valueKeys :: ValueKeys
			}

instance Product (AuditEvent ApplyMsg) PMEnv where
  proj = applyMsg
  upd v env = env { applyMsg = v }

instance Product (AuditEvent LabelMsg) PMEnv where
  proj = labelMsg
  upd v env = env { labelMsg = v }

instance Product ValueKeys PMEnv where
  proj = valueKeys
  upd v env = env { valueKeys = v }

data PMExc = TrivE Triv 
	   | TimeoutE Timeout
	deriving Show

instance Sum Triv PMExc where
  inj = TrivE
  sel (TrivE v) = Just v
  sel _         = Nothing

instance Sum Timeout PMExc where
  inj = TimeoutE
  sel (TimeoutE v) = Just v
  sel _         = Nothing

instance Product ValueKeys env => Applicator (TestM env s exc) where
  getArgument f        = TestM $ \ env st -> case proj env of
					      ValueKeys (vk:vks) -> unTestM (f vk) (upd (ValueKeys vks) env) st
  applyArguments vks m = TestM $ \ env st -> unTestM m (upd vks env) st

instance Product R.StdGen state => RandomMonad (TestM env state exc) where 
  getStdGen = TestM $ \ env st -> let (r1,r2) = R.split (proj st) in return (Right r1,upd r2 st)

------------------------------------------------------------------------------

newtype P a = P { unP :: PM Bool }

runtest :: (Testable t) => t -> IO ()
runtest the_test =
  do let p@(P m) = property the_test
     stdGen <- R.getStdGen
     print stdGen


     let test = timingM
	     (
	       (do stdGen <- getStdGen
                   r <- timeoutM 1 (applySeriesArgs stdGen p args)
		   return (Right r)) `catchError` (\ e -> return (Left e)))

     let three xs = init xs ++ ["." ++ last xs]
            where txt = xs ++ xs ++ xs
     let spin = three ["|","/","-","\\"] ++ spin
     spins <- newMVar spin
     
     let doPrint = do (c:cs) <- takeMVar spins
                      putMVar spins cs
                      putStr c
                      putStr "\b"
                      hFlush stdout

     let print' (TestArgs s vks) = doPrint -- print (showArgValues s vks) -- doPrint

     labProc <- processLabels $ (putStrLn . percentages)

     let runTrans :: R.StdGen -> PM a -> IO ()
         runTrans stdGen (TestM m) = do
	  (r,_) <- m (PMEnv (AuditEvent print') (AuditEvent labProc) (ValueKeys [])) (PMState stdGen)
--          labProc ShowLabels
	  case r of
	    Left e -> do fail $ show e
            Right v -> return ()

     let loop n = doWhile (0,0) test $ \ (a,tm) (good,triv) -> do
             case a of 
		Right True -> let good' = succ good in
		      	      if good' >= n 
			      	 then return (Left ())
			      	 else return (Right (good',triv))
		Right False -> return (Left ())
		Left (TrivE {})
		         -> do 
			       let triv' = succ triv 
		      	       if triv' >= n * 10
			      	 then return (Left ())
			      	 else return (Right (good,triv'))
		Left (TimeoutE {})
		         -> do let triv' = succ triv 
		      	       if triv' >= n * 10
			      	 then return (Left ())
			      	 else return (Right (good,triv'))
     v1 <- newEmptyMVar
     v2 <- newEmptyMVar
     let (r1,r2) = R.split stdGen
     p1 <- forkIO $ runTrans r1 (loop 50) >> putMVar v1 ()
     p2 <- forkIO $ runTrans r2 (loop 50) >> putMVar v2 ()
     takeMVar v1
     takeMVar v2

--print' :: ApplyMsg -> IO ()
--print' (TestArgs s vks) = print (showArgValues s vks)
--print' (TestResult True) = print "."
--print' (TestResult False) = print "*"


instance Property PM P where	-- still not sure about this one
  rep = unP
  abs = P

instance SerialArgs a => TestableWith PM P (P a) where -- also generic???
  property p = abs (rep p)

instance SerialArgs t => SerialArgs (P t) where	-- perhaps could make this generic
  args = Prop args 

class (TestableWith PM P t) => Testable t 
instance (TestableWith PM P t) => Testable t

-- speicalize the types for the DSL export
(==>) :: (Testable t) => Bool -> t -> P t
(==>) = (U.==>)

label :: (Testable t) => String -> t -> P t
label  msg t = P $ do report (Label msg)
                      unP (property t)

------------------------------------------------------------------------------

data CheckitTest = forall t . (Testable t) => String :~> t 

checkit :: [CheckitTest] -> IO ()
checkit tests = 
        sequence_ [ do print name
                       runtest the_test
                       print "[DONE]" 
                  | name :~> the_test <- tests
                  ]
        
check :: (Testable t) => t -> IO ()
check t = checkit ["(prop_...)" :~> t]


