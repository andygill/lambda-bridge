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
import Network.LambdaBridge.Multiplex
import Network.LambdaBridge.Frame
import Data.Char

import qualified Data.Map as Map

import Data.Time.Clock

-- Set up an end to end to test things.

main :: IO ()
main = do
                                                -- 10 bits at 9600 baud
	(bridge_byte_lhs,bridge_byte_rhs) <- pipeBridge 16 (10/9600) :: IO (Bridge Byte, Bridge Byte)	

	let u = def { pauseU = 0.001
		    , loseU = 0.000, dupU = 0.000, mangleU = 0.01, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte_lhs <- realisticBridge u u bridge_byte_lhs
--	bridge_byte_rhs <- realisticBridge u def bridge_byte_rhs

--	bridge_byte_lhs <- debugBridge "bridge_byte_lhs" bridge_byte_lhs
--	bridge_byte_rhs <- debugBridge "bridge_byte_rhs" bridge_byte_rhs
	
	bridge_frame_lhs <- frameProtocol bridge_byte_lhs
	bridge_frame_rhs <- frameProtocol bridge_byte_rhs

	bridges_frame_lhs <- multiplexBridge [0x99,0x100] bridge_frame_lhs
	bridges_frame_rhs <- multiplexBridge [0x99,0x100] bridge_frame_rhs

	let bridge_frame_lhs = bridges_frame_lhs 0x99
	let bridge_frame_rhs = bridges_frame_rhs 0x99


--	bridge_frame_lhs <- debugBridge "bridge_frame_lhs" bridge_frame_lhs
	

	send <- sendWithARQ bridge_frame_lhs $ Limit 1 $ \ t o -> 
			case o of
			   Nothing -> min (t * 2) 10
			   Just a  -> (a * 4 + t) / 2
	
	recv <- recvWithARQ bridge_frame_rhs

        stop <- newEmptyMVar

	forkIO $ let loop 1000 = putMVar stop ()
	             loop n = do
			send (toStr $ show (n :: Int))
			loop (succ n)
		 in loop 0

	forkIO $ let loop = do
			msg <- recv
			print ("MSG",msg)
--			threadDelay (100 * 1000)
			loop
		 in loop

        takeMVar stop
--	threadDelay (1000 * 1000 * 1000)

	return ()
   where	
	toStr :: String -> Link
	toStr = Link . BS.pack . map (fromIntegral . ord)
