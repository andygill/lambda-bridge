{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Network.LambdaBridge.Bridge where

import Data.Word as W
import System.Random
import Data.Default
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Numeric
import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time.Clock
import Control.Exception as Exc
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import System.IO.Unsafe (unsafeInterleaveIO)

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge msg = Bridge 
	{ toBridge 	:: msg -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO msg	-- ^ read from a bridge; may block, called many times.
					--   The expectation is that *eventually* after some
					--   time and/or computation
					--   someone will read from the bridge, and the
					--   reading does not depend on any external interaction or events.
	}

--------------------------------------------------------------------------

-- | A 'Bridge (of) Byte' is for talking one byte at a time, where the
-- byte may or may not get there, and may get garbled.
--
-- An example of a 'Bridge (of) Byte' is a RS-232 link.

newtype Byte = Byte W.Word8 deriving (Eq,Ord)

instance Show Byte where
   show (Byte w) = "0x" ++ showHex w ""

--------------------------------------------------------------------------
-- | A 'Bridge (of) Frame' is small set of bytes, where a Frame may
-- or may not get to the destination, but if received, will 
-- not be garbled or fragmented (via CRC or equiv).
-- There is typically an implementation specific maximum size of a Frame.

-- An example of a 'Bridge (of) Frame' is UDP.

newtype Frame = Frame BS.ByteString 
instance Show Frame where
   show (Frame wds) = "Frame " ++ show [ Byte w | w <- BS.unpack wds ]

-- | A way of turning a Frame into its contents, using the 'Binary' class.
-- This may throw an async exception.
fromFrame :: (Binary a) => Frame -> a
fromFrame (Frame fs) = decode (LBS.fromChunks [fs])

-- | A way of turning something into a Frame, using the 'Binary' class.
toFrame :: (Binary a) => a -> Frame
toFrame a = Frame $ BS.concat $ LBS.toChunks $ encode a

--------------------------------------------------------------------------

-- | 'debugBridge' outputs to the stderr debugging messages
-- about what datum is getting send where.

debugBridge :: (Show msg) => String -> Bridge msg -> IO (Bridge msg)
debugBridge name bridge = do
	sendCounter <- newMVar 0
	recvCounter <- newMVar 0

	return $ Bridge
		{ toBridge = \ a -> do
			count <- takeMVar sendCounter
			putMVar sendCounter (succ count)
			putStrLn $ name ++ ":toBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			() <- toBridge bridge a
			putStrLn $  name ++ ":toBridge<" ++ show count ++ "> success"
		, fromBridge = do
			count <- takeMVar recvCounter
			putMVar recvCounter (succ count)
			putStrLn $  name ++ ":fromBridge<" ++ show count ++ ">"
			a <- fromBridge bridge `Exc.catch` \ (e :: SomeException) -> do { print e ; throw e }
			putStrLn $  name ++ ":fromBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			return a
		}


-- |  ''Realistic'' is the configuration for ''realisticBridge''.
data Realistic a = Realistic
	{ loseU 	:: Float	-- ^ lose an 'a'
	, dupU 		:: Float	-- ^ dup an 'a'
	, execptionU 	:: Float	-- ^ throw exception instead
	, pauseU	:: Float	-- ^ what is the pause between things
	, mangleU 	:: Float	-- ^ mangle an 'a'
	, mangler       :: Float -> a -> a -- ^ how to mangle, based on a number between 0 and 1
	}

-- | default instance of 'realistic', which is completely reliable.
instance Default (Realistic a) where
	def = Realistic 0 0 0 0 0 (\ g a -> a)

connectBridges :: (Show msg) => Bridge msg -> Realistic msg -> Realistic msg -> Bridge msg -> IO ()
connectBridges lhs lhsOut rhsOut rhs = do
	let you :: Float -> IO Bool
	    you f = do
		r <- randomIO
		return $ f > r

	let optMangle f mangle a = do
		b <- you f
		if b then do
		        r <- randomIO 
			return $ mangle r a
		     else return a 


        let unrely :: MVar UTCTime -> Realistic msg -> msg -> (msg -> IO ()) -> IO ()
            unrely tmVar opts a k = do
		tm0 <- takeMVar tmVar	-- old char time
		tm1 <- getCurrentTime	-- current time
		let pause = pauseU opts - realToFrac (tm1 `diffUTCTime` tm0)
		if pause <= 0 then return () else do
		   threadDelay (floor (pause * 1000 * 1000))
		   return ()
		b <- you (loseU opts)
		if b then return () else do -- ignore if you "lose" the message.
		  a <- optMangle (mangleU opts) (mangler opts) a
		  b <- you (dupU opts)
		  if b then do		   -- send twice, please
		  	k a			
		  	k a			
		       else do
	                k a
		tm <- getCurrentTime
		putMVar tmVar tm
		return ()

	tm <- getCurrentTime
	tmVar1 <- newMVar tm
	tmVar2 <- newMVar tm

        forkIO $ forever $ do
                msg <- fromBridge lhs
                unrely tmVar1 lhsOut msg $ toBridge rhs
        forever $ do
                msg <- fromBridge rhs
                unrely tmVar2 rhsOut msg $ toBridge lhs
