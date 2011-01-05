-- Tests of the frame bridge

import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import System.IO
import Data.Time.Clock
import Data.Default
import Control.Monad


import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ

-- TMP
import Network.LambdaBridge.Frame
import Data.Char

-- Set up an end to end to test things.

main :: IO ()
main = do
	bridge_byte_lhs0 <- stubBridge $ [] -- cycle [Byte n | n <- [0..255]]

	(bridge_byte_lhs,bridge_byte_rhs) <- pipeBridge :: IO (Bridge Byte, Bridge Byte)	

	let u = def { pauseU = 0.001
		    , loseU = 0.001, dupU = 0.001, mangleU = 0.005, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte_lhs <- realisticBridge u def bridge_byte_lhs
--	bridge_byte_rhs <- realisticBridge u def bridge_byte_rhs

--	bridge_byte_lhs <- debugBridge "bridge_byte_lhs" bridge_byte_lhs
--	bridge_byte_rhs <- debugBridge "bridge_byte_rhs" bridge_byte_rhs
	
	bridge_frame_lhs <- frameProtocol bridge_byte_lhs
	bridge_frame_rhs <- frameProtocol bridge_byte_rhs

	bridge_frame_lhs <- debugBridge "bridge_frame_rhs" bridge_frame_lhs

	send <- sendWithARQ bridge_frame_lhs $ Timeout 1 $ \ t o -> 
			case o of
			   Nothing -> min (t * 2) 10
			   Just a  -> (a * 4 + t) / 2
	
	recv <- recvWithARQ bridge_frame_rhs

	forkIO $ let loop n = do
			send (toStr $ show (n :: Int))
			loop (succ n)
		 in loop 0

	forkIO $ let loop = do
			msg <- recv
			print msg
			threadDelay (1000 * 1000)
			loop
		 in loop

	threadDelay (1000 * 1000 * 1000)

	return ()
   where	
	toStr :: String -> BS.ByteString
	toStr = BS.pack . map (fromIntegral . ord)

