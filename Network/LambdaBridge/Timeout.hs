
module Network.LambdaBridge.Timeout where

import Control.Concurrent.MVar
import Control.Exception
import Data.Time.Clock
import qualified System.Timeout as T

-- | 'Timeout' is a generic way of having adaptive timeouts.
data Timeout f = Timeout 
	{ waitFor :: f				-- ^ how long to wait
	, newTimeout :: f -> Maybe f -> f	-- ^ given an old timeout time, 
						-- and the actual time taken (or timeout)
						-- how long should we wait?
	}

-- | 'timeout' uses these adaptive timeouts.
timeout :: Timeout Double -> IO (IO a -> IO (Maybe a))
timeout (Timeout first fn) = do
	timeVar <- newMVar first
	return $ \ comp -> do
	   waitFor <- takeMVar timeVar
	   print ("Wait for : ", waitFor)
	   (do  tm0 <- getCurrentTime
		res <- T.timeout (floor (waitFor * 1000 * 1000)) comp
		case res of
		   Nothing -> do putMVar timeVar $ fn waitFor Nothing
				 return Nothing
		   Just v -> do tm1 <- getCurrentTime
				putMVar timeVar $ fn waitFor (Just (realToFrac $ tm1 `diffUTCTime` tm0))
				return (Just v)) 
				
		`onException` 
				(putMVar timeVar $ fn waitFor Nothing)

