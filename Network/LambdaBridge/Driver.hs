module Network.LambdaBridge.Driver where

import System.Environment
import GHC.IO.Handle.FD
import System.IO 
import Control.Monad
import Data.Char(isDigit)
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.Multiplex
import Network.LambdaBridge.ARQ


-- | 'bundle_driver' builds a driver. This should be called via board_connect,
--  so a typical use is
--
-- > main = bundle_driver "driver-name/description" $ \ args ins outs -> do
-- >    		-- ...
-- >			-- work to talk to the board
-- >			-- ...

-- The functional argument takes command like arguments and Handles,
-- and runs something, typically talking to a board, or virtual hardware.

bundle_driver :: String -> ([String] -> [Handle] -> [Handle] -> IO ()) -> IO ()
bundle_driver name cont = do
	args <- getArgs
	case args of
	  ("1":n:"1":m:rest) | all isDigit n && all isDigit m -> do
		send <- fdToHandle (read n)
		resv <- fdToHandle (read m)
		hSetBuffering send NoBuffering
		hSetBuffering resv NoBuffering
		cont rest [send] [resv]
	  _ -> error $ "bad (or unsupported) argument format for driver (" ++ name ++ "): " 
			++ show args ++ "\n" ++ "(use 'board_connect' to call this driver)"
	
-- | 'bridge_frame_driver' creates a driver from the 'Bridge Frame' abstraction.

bridge_frame_driver :: String -> Limit -> ([String] -> IO (Bridge Frame)) -> IO ()
bridge_frame_driver name limit bridge_fn = bundle_driver name $ \ args ins out -> do
	bridge_frame <- bridge_fn args
	bridge_frames <- multiplexBridge [0x33,0x34] bridge_frame

	let bridge_frame_0x33 = bridge_frames 0x33
	let bridge_frame_0x34 = bridge_frames 0x34

	sendARQ <- sendWithARQ bridge_frame_0x33 limit

	recvARQ <- recvWithARQ bridge_frame_0x34

	-- Send first volley, to start the service
	sendARQ (Link $ BS.pack [])

	return ()

	forever $ do
		link <- recvARQ
		print link


{-




		forkIO $ 
		   let loop = do
			hWaitForInput s (-1)
			bs <- BS.hGetNonBlocking s 1024
			sendByteString protocol (1,bs)
			loop 
		   in loop
		
		let loop = do
			(chanId,bs) <- recvByteString protocol
			case chanId of
				0 -> print ("CONTROL:",bs)
			 	1 -> BS.hPut r bs
			loop
		
		loop
-}
		
{-
driver_timeout :: Timeout Float
driver_timeout 
-}
