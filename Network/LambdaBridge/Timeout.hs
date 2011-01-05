module Network.LambdaBridge.Timeout where

import Control.Concurrent.MVar
import Control.Exception
import Data.Time.Clock
import qualified System.Timeout as T

-- | 'Limit' is a generic way of having adaptive timeouts, in (fractions of) seconds.
data Limit = Limit 
	{ waitFor :: Float			-- ^ how long to wait, in seconds
	, newLimit :: Float -> Maybe Float -> Float
						-- ^ given an old timeout time, 
						-- and the actual time taken (or timeout)
						-- how long should we wait?
	}

-- | 'boundLimit' creates a Limit with a specific upper bound.
boundLimit :: Float -> Limit
boundLimit n = Limit (n / 10) $ \ t o -> 
	case o of
	   Nothing -> min (t * 2) n
	   Just a  -> (a * 4 + t) / 2

-- | 'timeout' uses these Limits to create adaptive timeouts.
timeout :: Limit -> IO (IO a -> IO (Maybe a))
timeout (Limit first fn) = do
	timeVar <- newMVar first
	return $ \ comp -> do
	   waitFor <- takeMVar timeVar
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

