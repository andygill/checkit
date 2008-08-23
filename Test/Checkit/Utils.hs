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

------------------------------------------------------------------------------

------------------------------------------------------------------------------

doWhile :: (Monad m) => t1 -> m t -> (t -> t1 -> m (Either r t1)) -> m r
doWhile s m cont = do
	r <- m
        go <- cont r s
	case go of
	  Left r -> return r
	  Right s' -> doWhile s' m cont

	
	
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

