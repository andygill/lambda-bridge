{-# LANGUAGE RankNTypes #-}

module Foreign.LambdaBridge.Bridge where

import Data.Word as W
import qualified Data.ByteString as BS
import System.Random
import Data.Default
import Control.Concurrent
import Control.Concurrent.MVar
import Numeric

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge port a = Bridge 
	{ toBridge 	:: port -> a -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: port -> IO a		-- ^ read from a bridge; may block, called many times.
	}

{-
-- | A 
data Pipe msg resp = Bridge 
	{ toBridge 	:: port -> a -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: port -> IO a		-- ^ read from a bridge; may block, called many times.
	}
-}

-- | A 'BridgePort' is the port number of a remote FIFO/pipe,
-- where  0 is (reserved) controller, 1 is the main fifo.

type BridgePort = Word8

-- | A 'Bridge (of) () Byte' is for talking one byte at a time, where the
-- byte may or may not get there, and may get garbled.
--
-- An example of a 'Bridge (of) Byte' is a RS-232 link.

newtype Byte = Byte W.Word8 deriving (Eq,Ord)

instance Show Byte where
   show (Byte w) = "0x" ++ showHex w ""

-- | A 'RegisterAddr' is used for the LAD bus to give the address of a remote register.
type RegisterAddr = Word32

-- | A 'Bridge (of) RegisterAddr Register' is for reading/writing one register at a time, 
-- where the data will always get there, and will never be damaged.
--
-- An example of a 'Bridge (of) RegisterAddr Register' is the LAD bus API.
newtype Register = Register W.Word32 deriving (Eq,Ord)

instance Show Register where
   show (Register w) = "0x" ++ showHex w ""

-- | A 'Bridge (of) () Frame' is small set of bytes, where a Frame may
-- or may not get to the destiation, but if receieved, will 
-- not be garbled, and will not be fragmented.
-- There is typically an implementation specific maximum size of a Frame.

-- An example of a 'Bridge (of) Frame' is UDP.

newtype Frame = Frame BS.ByteString

-- | A 'Bridge (of) BridgePort Link' is an arbitary sized sequence of bytes,
-- that will be send to specific FIFO/pipe location at the destination,
-- without garble, and the protocol will keep trying until the payload
-- has been delivered.
-- 
-- The reason that we choose to handle ''BridgePort'' in the ''Link''
-- layer is due to one port may be blocked (the FIFO full),
-- so other writes to other ports need to succeed.

data Link = Link BS.ByteString

-- | ''unreliableBridge'' is a way of making a 'Bridge' less
-- reliable, for testing purposes.
unreliableBridge :: Unreliable a -> Unreliable a -> Bridge p a -> IO (Bridge p a)
unreliableBridge send recv sock = return $ Bridge
	{ toBridge = toBridge sock
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

debugBridge :: (Show p) => (a -> String) -> Bridge p a -> IO (Bridge p a)
debugBridge theShow bridge = do
	sendCounter <- newMVar 0
	recvCounter <- newMVar 0

	return $ Bridge
		{ toBridge = \ p a -> do
			count <- takeMVar sendCounter
			putMVar sendCounter (succ count)
			putStrLn $ "toBridge<" ++ show count ++ "> (" ++ theShow a ++ "=>" ++ show p ++")"
			() <- toBridge bridge p a
			putStrLn $ "toBridge<" ++ show count ++ "> success (" ++ show p ++ ")"
		, fromBridge = \ p -> do
			count <- takeMVar recvCounter
			putMVar recvCounter (succ count)
			putStrLn $ "fromBridge<" ++ show count ++ "> (" ++ show p ++")"
			a <- fromBridge bridge p
			putStrLn $ "fromBridge<" ++ show count ++ "> (" ++ theShow a ++ "<=" ++ show p ++")"
			return a
		}

nullBridge :: IO (Bridge () a)
nullBridge = do
	var <- newEmptyMVar
	return $ Bridge
		{ toBridge = \ () a -> putMVar var a
		, fromBridge = \ () -> takeMVar var
		}
		
	
