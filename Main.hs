module Main where

import Control.Applicative
import System.Random
import Data.List
import Control.Monad
import Control.Monad.Trans

import Test.Checkit.Serial
import Test.Checkit.ValueKey
import Test.Checkit.Testable
import Test.Checkit.Interfaces as I
import Test.Checkit.Check
import Test.Checkit.TestMonad

import GHC.Conc
--import Control.Concurrent
--import System.CPUTime	

prop_map_id xs = label (show xs) (xs == map id xs)
   where
	types = (xs :: [Int])


prop_map_sort xs = label (show xs) (map id (sort xs) == sort (map id xs))
   where
	types = (xs :: [Int])

prop_len xs ys = length (xs ++ ys) == length xs + length ys
  where types = (xs :: [Bool])

prop_fib xs = not (null xs) ==> (fib n == length (fibL xs3))
  where types = (xs :: [()])
	n     = length xs3
	xs1   = xs ++ xs
	xs2   = xs1 ++ xs1
	xs3   = take 25 (cycle xs2)

fibL      :: [()] -> [()]
fibL []   = [()]
fibL [()] = [()]
fibL n    = fibL (tail n) ++ fibL (tail (tail n))

fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

-- intentually wrong!
prop_rev xs ys = 
	 not (null xs) ==> (reverse (xs ++ ys) == reverse xs ++ reverse ys)
  where types = (xs :: [Bool])

prop_hack xs ys zs = 
	    True ==> \ (a,b) t -> let tt = (a::Bool,b::Bool,t::()) in True ==> True
   where types = (xs :: [()],ys::(),zs::Bool)

------------------------------------------------------------------------------

{-
prop_sort xs = length xs >=> sort xs
    where
	types = (xs :: [(Bool,Bool)])

-}

prop_io x = (return () :: IO ())
     where
        types = (x :: Bool)

prop_io2 x = (return True :: IO Bool)
     where
        types = (x :: Bool)

prop_write_read_IO str = do pid <- myThreadId
                            let file = "/tmp/foo" ++ show pid
                            writeFile file msg
                            threadDelay 1000
                            msg2 <- readFile file
                            return $ msg == msg2
  where
        types = (str :: [Int])
        msg  = show types


main = do
     print numCapabilities
     tm1 <-  getCPUTimeSec 
     checkit
     	     [ "prop_map_id" :~> prop_map_id
	     , "prop_hack"   :~> prop_hack
	     , "prop_rev"    :~> prop_rev
             , "prop_fib"  :~> prop_fib
             , "prop_io"     :~> prop_io
             , "prop_io2"    :~> prop_io2
--             , "prop_write_read_IO" 
--                             :~> prop_write_read_IO
             ]
     tm2 <- getCPUTimeSec
     print (tm2 - tm1)

{-
fib :: Int -> Int
fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

fibL      :: [a] -> [a]
fibL []   = [undefined]
fibL [a] = [a]
fibL n    = fibL (tail n) ++ fibL (tail (tail n))


main = do
     print numCapabilities
     let n = 10
     tm1 <-  getCPUTimeSec 
     vs <- sequence [ newEmptyMVar | n <- [1..n]]
     ps <- sequence [ forkIO $ putMVar v $! ((fib (length (take 37 (repeat v)))))
     	   	               | v <- vs 
			       ]
     sequence_ [ takeMVar v | v <- vs ]
     tm2 <- getCPUTimeSec
     print (tm2 - tm1)


getCPUTimeSec :: IO Double
getCPUTimeSec = do
    t <- getCPUTime
    return (fromIntegral t / (100 * fromIntegral cpuTimePrecision))
-}
