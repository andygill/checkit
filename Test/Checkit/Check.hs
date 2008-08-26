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
import Test.Checkit.Text
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent
import System.IO

import GHC.Conc

import Prelude hiding (abs)

------------------------------------------------------------------------------

type PM = TestM PMEnv PMState PMExc

newtype PMState = PMState { rnd :: R.StdGen }

instance Product R.StdGen PMState where
  proj (PMState v) = v
  upd v (PMState _) = PMState v

instance SplitState PMState where
   splitState (PMState r) = (PMState r1,PMState r2)
     where (r1,r2) = R.split r


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

data RejectMsg = RejectMsg deriving Show

runtest :: Output -> (Testable t) => t -> IO ()
runtest out the_test =
  do let p@(P m) = property the_test
     stdGen <- R.getStdGen

     args' <- return $ args	-- forcing monomorphic
     
     -- Only the non-IO can be run in parallel
     let par_count = if isIO args' then 1 else numCapabilities
--     when (isIO args') $ 
--          writeOutput out "[IO] "

     let test = timingM
	      ((do stdGen <- getStdGen
                   r <- timeoutM 1 (applySeriesArgs stdGen p args')
		   return (Right r)) `catchError` (\ e -> return (Left e)))

     labProc <- processLabels $ (putStrLn . percentages)

     let print' (TestArgs s vks) = return ()

     let bad 0 = ""
         bad n = ", (" ++ show n ++ " test(s) trivial)"

     let runTrans :: R.StdGen -> PM ((Int,Int),Maybe RejectMsg) -> IO ()
         runTrans stdGen (TestM m) = do
	  (r,_) <- m (PMEnv (AuditEvent print') (AuditEvent labProc) (ValueKeys [])) (PMState stdGen)
--          labProc ShowLabels
	  case r of
	    Left e -> do fail $ show e
            Right ((g,b),Nothing) -> writeOutput out $ "PASS" ++ bad b
            Right ((g,b),Just v)  -> writeOutput out $ "FAIL, " ++ show v

     let testCond n (a,tm) (good,triv) =
             case a of 
		Right True -> let good' = succ good in
		      	      if good' > n 
			      	 then (Left (Nothing))
			      	 else (Right (good',triv))
		Right False -> (Left (Just RejectMsg))
		Left (TrivE {})
		         -> do 
			       let triv' = succ triv 
		      	       if triv' >= n * 10
			      	 then (Left (Just RejectMsg))
			      	 else (Right (good,triv'))
		Left (TimeoutE {})
		         -> do let triv' = succ triv 
		      	       if triv' >= n * 10
			      	 then (Left (Just RejectMsg))
			      	 else (Right (good,triv'))

     let loop n = doWhile par_count (0,0) test (testCond n) (\ (g,b) -> liftIO $ writeTempOutput out (show (g `div` 5) ++ "%"))

     v1 <- newEmptyMVar
     v2 <- newEmptyMVar
     let cores = numCapabilities * 8	-- the idea is large jobs keep going when other jobs are done

     let counter = 500
     let counters0 = [ counter `div` cores | n <- [2..cores]]

     let counters1 = counter - sum counters0 : counters0

     -- perhaps should just fork each test? 100 threads is easy for GHC.

--     print counters0
--     print counters1

     let (r1,r2) = R.split stdGen
     forkIO $ do { runTrans r2 (loop 500) ; putMVar v1 () }
{-
     ps <- sequence [ do v1 <-newEmptyMVar
                         forkIO $ do { runTrans r2 (loop 100) ; putMVar v1 () }
                         return $ v1
                    ]
-}
--     p1 <- 
--     p2 <- forkIO $ runTrans r2 (loop 50) >> putMVar v2 ()
     takeMVar v1
--     takeMVar v2

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

instance TestableWith (TestM PMEnv PMState PMExc) P (IO ()) where
   property ioAction = abs (do { () <- liftIO ioAction; return $! True })

instance TestableWith (TestM PMEnv PMState PMExc) P (IO Bool) where
   property ioAction = abs (do { r <- liftIO ioAction ; return $! r })

--instance SerialArgs (IO ()) where	-- perhaps could make this generic
--  args = undefined

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
checkit tests = do
        out <- mkOutput stdout
        let max = maximum [ length name | name :~> the_test <- tests ]
        let tests2 = [ take max (name ++ cycle " ") :~> the_test 
                     | name :~> the_test <- tests 
                     ]
        sequence_ [ do writeOutput out (name ++ " : ")
                       runtest out the_test
                       writeOutput out "\n"
                  | name :~> the_test <- tests2
                  ]
        flushOutput out
        
check :: (Testable t) => t -> IO ()
check t = checkit ["checking..." :~> t]


