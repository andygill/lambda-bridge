{-# LANGUAGE RankNTypes #-}

module Network.LambdaBridge.Bridge where

import Data.Word as W
import qualified Data.ByteString as BS
import System.Random
import Data.Default
import Control.Concurrent
import Control.Concurrent.MVar
import Numeric

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge msg resp = Bridge 
	{ toBridge 	:: msg -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO resp	-- ^ read from a bridge; may block, called many times.
	}

-- | A 'BridgePort' is the port number of a remote FIFO/pipe,
-- where  0 is (reserved) controller, 1 is the main fifo.

type BridgePort = Word8

-- | A 'Bridge (of) Byte Byte' is for talking one byte at a time, where the
-- byte may or may not get there, and may get garbled.
--
-- An example of a 'Bridge (of) Byte Byte' is a RS-232 link.

newtype Byte = Byte W.Word8 deriving (Eq,Ord)

instance Show Byte where
   show (Byte w) = "0x" ++ showHex w ""

{-
-- | A 'RegisterAddr' is used for the LAD bus to give the address of a remote register.
type RegisterAddr = Word32

-- | A 'Bridge (of) RegisterAddr Register' is for reading/writing one register at a time, 
-- where the data will always get there, and will never be damaged.
--
-- An example of a 'Bridge (of) RegisterAddr Register' is the LAD bus API.
newtype Register = Register W.Word32 deriving (Eq,Ord)

instance Show Register where
   show (Register w) = "0x" ++ showHex w ""
-}

-- | A 'Bridge (of) Frame Frame' is small set of bytes, where a Frame may
-- or may not get to the destiation, but if receieved, will 
-- not be garbled, and will not be fragmented.
-- There is typically an implementation specific maximum size of a Frame.

-- An example of a 'Bridge (of) Frame Frame' is UDP.

newtype Frame = Frame BS.ByteString deriving (Show)

{-
-- | A 'Bridge (of) BridgePort Link' is an arbitary sized sequence of bytes,
-- that will be send to specific FIFO/pipe location at the destination,
-- without garble, and the protocol will keep trying until the payload
-- has been delivered.
-- 
-- The reason that we choose to handle ''BridgePort'' in the ''Link''
-- layer is due to one port may be blocked (the FIFO full),
-- so other writes to other ports need to succeed.

data Link = Link BS.ByteString
-}

-- | ''unreliableBridge'' is a way of making a 'Bridge' less
-- reliable, for testing purposes.
unreliableBridge :: Unreliable msg -> Unreliable resp -> Bridge msg resp -> IO (Bridge msg resp)
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

debugBridge :: (Show msg, Show resp) => Bridge msg resp -> IO (Bridge msg resp)
debugBridge bridge = do
	sendCounter <- newMVar 0
	recvCounter <- newMVar 0

	return $ Bridge
		{ toBridge = \ a -> do
			count <- takeMVar sendCounter
			putMVar sendCounter (succ count)
			putStrLn $ "toBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			() <- toBridge bridge a
			putStrLn $ "toBridge<" ++ show count ++ "> success"
		, fromBridge = do
			count <- takeMVar recvCounter
			putMVar recvCounter (succ count)
			putStrLn $ "fromBridge<" ++ show count ++ ">"
			a <- fromBridge bridge
			putStrLn $ "fromBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			return a
		}

-- | 'loopbackBridge' is a simple reflective loopback. It obeys the basic
-- permise of a bridge; if no-one is listening, then data can get lost.
-- It is in the nature of the bridge; things do not always make it.

loopbackBridge :: IO (Bridge a a)
loopbackBridge = do
	ready <- newEmptyMVar	-- if full, then this is the var someone is waiting on
	return $ Bridge
		{ toBridge = \ a -> do
			var <- takeMVar ready
			putMVar var a
{-
			var' <- tryTakeMVar ready
			case var' of
			  Just var -> putMVar var a
			  Nothing  -> do
				print "DROP"
				return ()
-}
		, fromBridge = do
			var <- newEmptyMVar
			putMVar ready var
			takeMVar var
		}
