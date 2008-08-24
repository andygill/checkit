{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}

module Test.Checkit.TestMonad where

import Control.Monad.Error

import Control.Concurrent
import Control.Exception
import System.CPUTime

import Test.Checkit.Interfaces
import Test.Checkit.Utils(ApplyMsg(..))

newtype TestM e s x a = TestM { unTestM :: e -> s -> IO (Either x a,s) }

instance Monad (TestM e s x) where 
  return e = TestM $ \ env st -> return (Right e,st)
  (TestM m) >>= k = TestM $ \ env st ->
  		        do (r,st') <- m env st
			   case r of
			     Left e -> return (Left e,st')
			     Right v -> unTestM (k v) env st'


instance (Sum one all) => MonadError one (TestM e s all) where
  throwError e = TestM $ \ _env st -> return (Left (inj e),st)
  catchError (TestM m) k = TestM $ \ env st ->
  		        do (r,st') <- m env st
			   case r of
			     Left e -> case sel e of
					Nothing -> return (Left e,st')
					Just ex -> unTestM (k ex) env st'
			     Right v -> return (Right v,st')


instance MonadIO (TestM e s x) where
  liftIO m = TestM $ \ e s -> do v <- m
				 return (Right v,s) 


------------------------------------------------------------------------------

data Timeout = Timeout
	deriving Show

timeoutM :: (Sum Timeout exc) => Double -> TestM e s exc a -> TestM e s exc a
timeoutM sec (TestM m) = TestM $ \ e s -> do
        mvar <- newEmptyMVar
        do pid1 <- forkIO $ do r <- m e s ; putMVar mvar (Just r)
	   finally (do pid2 <- forkIO $ do  threadDelay usec ; putMVar mvar Nothing 
                       finally (do res <- takeMVar mvar
                                   case res of
                                     Just r      -> return r
                                     Nothing     -> return (Left (inj Timeout),s) -- original state
                               )
                               (killThread pid2)   
	     	   )
                   (killThread pid1)	-- make sure this dies
  where
	usec = fromInteger (floor (sec * 1000 * 1000))

-- too generic for here, adapted from Haskell Wiki, needs made composable (an exception will leave orphins)

compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    (do tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
        result <- takeMVar mvar
        mapM_ killThread tids
        return result)


timingM :: TestM e s exc a -> TestM e s exc (a,Double)
timingM (TestM m) = TestM $ \ e s -> do
  t1 <- getCPUTimeSec
  (r,s') <- m e s
  case r of
   Left e -> return (Left e,s')
   Right v -> do
	t2 <- getCPUTimeSec
	return (Right (v,t2 - t1),s')


getCPUTimeSec :: IO Double
getCPUTimeSec = do
    t <- getCPUTime
    return (fromIntegral t / (100 * fromIntegral cpuTimePrecision))


------------------------------------------------------------------------------

-- TODO: generalize
instance Product (AuditEvent ApplyMsg) env => Auditor ApplyMsg (TestM env s exc) where
  report msg = TestM $ \ env st -> do { unAuditEvent (proj env) msg ; return (Right (),st) }

instance Product (AuditEvent LabelMsg) env => Auditor LabelMsg (TestM env s exc) where
  report msg = TestM $ \ env st -> do { unAuditEvent (proj env) msg ; return (Right (),st) }

------------------------------------------------------------------------------

class SplitState s where
   splitState :: s -> (s,s)

instance (SplitState s) => MonadFork (TestM e s x) where
  forkM m = TestM $ \ env st -> do let (st1,st2) = splitState st
  	    	      	     	   pid <- forkIO (unTestM m env st1 >> return ())	-- throws away any exceptions??
				   return (Right $ liftIO $ killThread pid ,st2)

