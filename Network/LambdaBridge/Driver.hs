module Network.LambdaBridge.Driver where

import System.Environment
import GHC.IO.Handle.FD
import Control.Concurrent
import System.IO 
import Control.Monad
import Data.Char(isDigit)
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.Multiplex
import Network.LambdaBridge.Frame
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
-- 
-- Typically, 'bridge_frame_driver' or 'bridge_byte_driver' will be used to
-- build a driver, from an existing bridge. 'bridge_link_driver' is only
-- used in the case of the 'basic' bridges, building on a reliable 'Link'.

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

-- | 'bridge_link_driver' assumes the two FIFO archtecture, one in each direction of the Link-Bridge; the simple case.
bridge_link_driver :: String -> Limit -> ([String] -> IO (Bridge Link)) -> IO ()
bridge_link_driver name limit bridge_fn = bundle_driver name $ \ args ins out -> do

	bridge_link <- bridge_fn args

	bridge_link <- debugBridge "bridge_frame_driver" bridge_link
        
        let sendARQ = toBridge bridge_link
            recvARQ = fromBridge bridge_link
            
	-- Send first volley, to start the service
	sendARQ (Link $ BS.pack [])

	print (ins,out)

	let hGetSome n = do
	 	b   <- BS.hGet (head ins) 1
		bss <- get (n - 1)
		return (BS.append b (BS.concat bss))
	      where
		get 0 = return []
		get n = do bs <- BS.hGetNonBlocking (head ins) n
			   if BS.null bs 
				then return []
				else do bss <- get (n - BS.length bs)
					return (bs:bss)

	let reader = do
		bs <- hGetSome 128	-- 128 is the size of our 'packets'
		if BS.null bs
		  then return ()
		  else do
			print ("sending",bs)
			sendARQ (Link bs)
			reader
		
	forkIO $ reader

	let writer = do
		hPutStrLn (head out) "Hello"
		threadDelay (1000 * 100)
		(Link bs) <- recvARQ
		print ("recv",bs)
		BS.hPut (head out) bs
		writer
		
		
	writer `catch` \ e -> print ("Exc",e)

	
-- | 'bridge_frame_driver' creates a driver from the 'Bridge Frame' abstraction,
-- for example a UDP implementation over RJ45.

bridge_frame_driver :: String -> Limit -> ([String] -> IO (Bridge Frame)) -> IO ()
bridge_frame_driver name limit bridge_fn = bundle_driver name $ \ args ins out -> do

	bridge_frame <- bridge_fn args

	bridge_frame <- debugBridge "bridge_frame_driver" bridge_frame

	bridge_frames <- multiplexBridge [0x33,0x34] bridge_frame

	let bridge_frame_0x33 = bridge_frames 0x33
	let bridge_frame_0x34 = bridge_frames 0x34

	sendARQ <- sendWithARQ bridge_frame_0x33 limit

	recvARQ <- recvWithARQ bridge_frame_0x34

	-- Send first volley, to start the service
	sendARQ (Link $ BS.pack [])

	print (ins,out)

	let hGetSome n = do
	 	b   <- BS.hGet (head ins) 1
		bss <- get (n - 1)
		return (BS.append b (BS.concat bss))
	      where
		get 0 = return []
		get n = do bs <- BS.hGetNonBlocking (head ins) n
			   if BS.null bs 
				then return []
				else do bss <- get (n - BS.length bs)
					return (bs:bss)

	let reader = do
		bs <- hGetSome 128	-- 128 is the size of our 'packets'
		if BS.null bs
		  then return ()
		  else do
			print ("sending",bs)
			sendARQ (Link bs)
			reader
		
	forkIO $ reader

	let writer = do
		hPutStrLn (head out) "Hello"
		threadDelay (1000 * 100)
		(Link bs) <- recvARQ
		print ("recv",bs)
		BS.hPut (head out) bs
		writer
		
		
	writer `catch` \ e -> print ("Exc",e)

-- | 'bridge_byte_driver' creates a driver from the 'Bridge Byte' abstraction,
-- for example an RS-232.
bridge_byte_driver :: String -> Limit -> ([String] -> IO (Bridge Byte)) -> IO ()
bridge_byte_driver name limit bridge_fn = bridge_frame_driver name limit $ \ args -> do
	bridge_byte <- bridge_fn args
	-- and return the higher level frame protocol	
        frameProtocol bridge_byte

