
-- | Textual API for presenting tests

module Test.Checkit.Text where

import System.IO
import Control.Concurrent.MVar

data Text = Write String | Back Int

data Output = Output 
     	            { h :: Handle
		    , outMsg :: MVar [Text]	-- what to write
		    }

writeOutput :: Output -> String -> IO ()
writeOutput (Output h outMsg) msg = do modifyMVar_ outMsg $ \ str -> return $ Write msg : str
                                       flushOutput (Output h outMsg)

wipeOutput :: Output -> Int -> IO ()
wipeOutput (Output h outMsg) n = modifyMVar_ outMsg $ \ str -> return $ Back n : str --  Write (take n (cycle " ")) : Back n : str

flushOutput :: Output -> IO ()
flushOutput (Output h outMsg) = modifyMVar_ outMsg (\ txts -> do
                                   putStr (process (reverse txts))
                                   hFlush h
                                   return [])
         -- at some point clean this up; to many \b's
   where process [] = ""
         process (Write str:rest) = str ++ process rest
         process (Back n :rest)   = [ '\b' | _ <- [1..n]] ++ process rest

mkOutput :: Handle -> IO Output
mkOutput h = do v <- newMVar []
--                hSetBuffering h NoBuffering
                return $ Output h v


writeTempOutput :: Output -> String -> IO ()
writeTempOutput out msg = do
                writeOutput out msg
                flushOutput out
                wipeOutput out (length msg)                                
