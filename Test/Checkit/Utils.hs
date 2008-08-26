{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, FlexibleInstances, TypeOperators, FunctionalDependencies, ExistentialQuantification  #-}


module Test.Checkit.Utils where

import Control.Monad.Error
import qualified System.Random as R
import Control.Concurrent
import Control.Exception
import System.CPUTime
import Data.List
import Numeric

import Test.Checkit.Serial
import Test.Checkit.ValueKey

import Test.Checkit.Interfaces as I
import GHC.Conc
------------------------------------------------------------------------------


doWhile :: MonadFork m 
	     => Int			-- number of concurrent bodies
             -> s
	     -> m t			-- this can actually be run concurrently
	     -> (t -> s -> Either r s)	-- the conditional
             -> (s -> m ())    		-- the message based on the new result (interim result)
	     -> m (s,r)			-- any threads created are destroyed
doWhile cap s m cond msgr = do
        var <- liftIO $ newEmptyMVar
        let loop m = m >> loop m
        pids <- sequence [ forkM $ loop $
                           do v <- m 
                              liftIO $ putMVar var v
                         | _ <- [1..cap]
                         ]
        let checks s = do
               r <- liftIO $ takeMVar var
               case cond r s of
                 Left r -> do sequence pids
                              return (s,r)
                 Right s' -> do msgr s'
                                checks s'
        checks s

	


	
------------------------------------------------------------------------------


data Triv = Triv deriving (Eq,Ord,Show)

instance Error Triv where
  noMsg = Triv

type TrivT = ErrorT Triv

runTrivT :: TrivT m a -> m (Either Triv a)
runTrivT = runErrorT

(==>) :: (TestableWith m prop a,MonadError Triv m) => Bool -> a -> prop a
(==>) False _ = I.abs (throwError Triv)
(==>) True  p = property p

------------------------------------------------------------------------------

data ApplyMsg = forall s . TestArgs (SeriesArgs s) ValueKeys

applySeriesArgs 
      :: ( TestableWith m p (p a)
         , Auditor ApplyMsg m
         ) => R.StdGen
	   -> p a 				-- 
           -> SeriesArgs (p a)
	   -> m Bool				-- 
applySeriesArgs stdGen p seriesArgs = m'
  where
        inside = rep p
        m' = do let vks = pickValueKeys stdGen seriesArgs
		report (TestArgs seriesArgs vks)
                r <- applyArguments vks inside
                return r


-------------------------------------------------------------------------------

-- This is the reactive idiom at work.
processLabels :: ([String] -> IO ()) -> IO (LabelMsg -> IO ())
processLabels forward = do
    mVar <- newMVar []
    return $ \ m -> case m of
                       Label msg -> do msgs <- takeMVar mVar
                                       putMVar mVar (msg : msgs)
                       ShowLabels -> do msgs <- takeMVar mVar
                                        putMVar mVar []
                                        forward msgs

                       
                                    
percentages :: [String] -> String
percentages xs 
            = unlines 
            . map (\ (x,y) -> perc x ++ " : " ++ y)
            . reverse
            . sortBy (\ a b -> compare (fst a) (fst b))
            . map (\ xs -> (length xs,head xs)) 
            . group 
            . sort
            $ xs
 where
        count = length xs
        perc n = showFFloat (Just 0) ((fromIntegral n / fromIntegral count) * 100) "%"

