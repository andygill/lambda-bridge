{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Network.LambdaBridge.Bridge where

import Data.Word as W
import System.Random
import Data.Default
import Control.Concurrent
import Control.Concurrent.MVar
import Numeric
import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS


import Control.Exception as Exc

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge msg = Bridge 
	{ toBridge 	:: msg -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO msg	-- ^ read from a bridge; may block, called many times.
					--   The expectation is that *eventually* after some
					--   time and/or compuation
					--   someone will read from the bridge, and the
					--   reading does not depend on any external interaction or events.
	}

-- | A 'BridgePort' is the port number of a remote FIFO/pipe,
-- where  0 is (reserved) controller, 1 is the main fifo.

type BridgePort = Word8

-- | A 'Bridge (of) Byte' is for talking one byte at a time, where the
-- byte may or may not get there, and may get garbled.
--
-- An example of a 'Bridge (of) Byte Byte' is a RS-232 link.

newtype Byte = Byte W.Word8 deriving (Eq,Ord)

instance Show Byte where
   show (Byte w) = "0x" ++ showHex w ""

-- | A 'Bridge (of) Frame' is small set of bytes, where a Frame may
-- or may not get to the destiation, but if receieved, will 
-- not be garbled or fragmented (via CRC or equiv).
-- There is typically an implementation specific maximum size of a Frame.

-- An example of a 'Bridge (of) Frame Frame' is UDP.

newtype Frame = Frame BS.ByteString deriving (Show)

-- | A way of turning a Frame into its contents, using the 'Binary' class.
-- This may throw an async exception.
fromFrame :: (Binary a) => Frame -> a
fromFrame (Frame fs) = decode (LBS.fromChunks [fs])

-- | A way of turning something into a Frame, using the 'Binary' class.
toFrame :: (Binary a) => a -> Frame
toFrame a = Frame $ BS.concat $ LBS.toChunks $ encode a

-- | ''unreliableBridge'' is a way of making a 'Bridge' less
-- reliable, for testing purposes.
unreliableBridge :: Unreliable msg -> Unreliable msg -> Bridge msg -> IO (Bridge msg)
unreliableBridge send recv sock = do
	let you :: Float -> IO Bool
	    you f = do
		r <- randomIO
		return $ f > r

	let optMangle f mangle a = do
		b <- you f
		if b then do
			g <- newStdGen
			return $ mangle g a
		     else return a 

  	return $ Bridge
	  { toBridge = \ a -> do
		b <- you (loseU send)
		if b then return () else do -- ignore if you "lose" the message.
		  a <- optMangle (mangleU send) (mangler send) a
		  b <- you (dupU send)
		  if b then do		   -- send twice, please
		  	toBridge sock a			
		  	toBridge sock a			
		       else do
	                toBridge sock a
	  , fromBridge = fromBridge sock
	  }

-- |  ''Unreliable'' is the configuration for ''unreliableBridge''.
data Unreliable a = Unreliable
	{ loseU 	:: Float	-- ^ lose an 'a'
	, dupU 		:: Float	-- ^ dup an 'a'
	, execptionU 	:: Float	-- ^ throw exception instead
	, mangleU 	:: Float	-- ^ mangle an 'a'
	, mangler :: forall g . (RandomGen g) => g -> a -> a
	}

-- | default instance of 'Unreliable', which is completely reliable.
instance Default (Unreliable a) where
	def = Unreliable 0 0 0 0 (\ g a -> a)

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
			a <- fromBridge bridge `Exc.catch` \ (e :: SomeException) -> do { print e ; fail "X" }
			putStrLn $  name ++ ":fromBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			return a
		}

-- | 'loopbackBridge' is a simple reflective loopback. It obeys the basic
-- permise of a bridge; if no-one is listening, then data can get lost.
-- It is in the nature of the bridge; things do not always make it.

loopbackBridge :: IO (Bridge a)
loopbackBridge = do
	var <- newEmptyMVar
	return $ Bridge
		{ toBridge = \ a -> do
			putMVar var a
		, fromBridge = do
			takeMVar var
		}

-- | 'pipeBridge' create two ends of a polymorphic 'Bridge'.

pipeBridge :: IO (Bridge a, Bridge a)
pipeBridge = do
	sending <- newEmptyMVar 
	recving <- newEmptyMVar
	let b1 = Bridge
		{ toBridge = \ a -> do
			putMVar sending a
		, fromBridge = do
			takeMVar recving
		}
	let b2 = Bridge
		{ toBridge = \ a -> do
			putMVar recving a
		, fromBridge = do
			takeMVar sending
		}

	return (b1,b2)


stubBridge :: [a] -> IO (Bridge a)
stubBridge recv = do
	var <- newMVar recv
	return $ Bridge
		{ toBridge = \ a -> return ()
		, fromBridge = do
			(r:rs) <- takeMVar var
			putMVar var rs
			return r
		}
	