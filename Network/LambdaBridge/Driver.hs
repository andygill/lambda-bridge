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


-- | 'bundle_driver' builds a driver.  A driver is a stand-alone UNIX process.
-- A Haskell program should never call bundle_driver directly, 
--  so a typical use is
--
-- > main = bundle_driver "driver-name/description" $ \ args ins outs -> do
-- >	-- ...
-- >	-- work to talk to the board
-- >	-- ...
--
-- The functional argument takes command like arguments and Handles,
-- and runs something, typically talking to a board, or virtual hardware.
-- 
-- Typically, 'bridge_frame_driver' or 'bridge_byte_driver' will be used to
-- build a driver, from an existing bridge. 'packet_driver' is only
-- used in the case of the 'basic' bridges, building on a reliable 'Packet'.
--
-- Drivers are invoked using board_connect, which does a UNIX fork/exec for you.

bundle_driver :: String -> ([String] -> [Handle] -> [Handle] -> IO ()) -> IO ()
bundle_driver name cont = do
	args <- getArgs
	case args of
        -- TODO: generalize to any number of channels.
	  ("1":n:"1":m:rest) | all isDigit n && all isDigit m -> do
		send <- fdToHandle (read n)
		resv <- fdToHandle (read m)
		hSetBuffering send NoBuffering
		hSetBuffering resv NoBuffering
		cont rest [send] [resv]
	  _ -> error $ "bad (or unsupported) argument format for driver (" ++ name ++ "): " 
			++ show args ++ "\n" ++ "(use 'board_connect' to call this driver)"

-- | 'packet_driver' assumes the two FIFO architecture (for now),
-- one in each direction of the Data-Bridge; the simple case.
packet_driver :: String -> ([String] -> Int -> Int -> IO ([Packet -> IO ()], [IO Packet])) -> IO ()
packet_driver name bridge_fn = bundle_driver name $ \ args ins outs -> do


        -- We assume single FIFO in each direction, 
        ([sendARQ],[recvARQ]) <- bridge_fn args (length ins) (length outs)

--	bridge_link <- debugBridge "bridge_frame_driver" bridge_link
--        
--        let sendARQ = toBridge bridge_link
--            recvARQ = fromBridge bridge_link
            
	-- Send first volley, to start the service
	sendARQ (Packet $ BS.pack [])

	print (ins,outs)

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
			sendARQ (Packet bs)
			reader
		
	forkIO $ reader

	let writer = do
		hPutStrLn (head outs) "Hello"
		threadDelay (1000 * 100)
		(Packet bs) <- recvARQ
		print ("recv",bs)
		BS.hPut (head outs) bs
		writer
		
		
	writer `catch` \ e -> print ("Exc",e)

	
-- | 'bridge_frame_driver' creates a driver from the 'Bridge Frame' abstraction,
-- for example a UDP implementation over RJ45.

bridge_frame_driver :: String -> Limit -> ([String] -> IO (Bridge Frame)) -> IO ()
bridge_frame_driver name limit bridge_fn = packet_driver name $ \ args in_sz out_sz -> do

        let inputs = take in_sz [0..]
        let outputs = take out_sz [(last inputs + 1) .. ]

	bridge_frame <- bridge_fn args

	bridge_frame <- debugBridge "bridge_frame_driver" bridge_frame

	bridge_frames <- multiplexBridge (inputs ++ outputs) bridge_frame

	sendARQs <- sequence 
	                [ sendWithARQ (bridge_frames i) limit
	                | i <- inputs
	                ]

	recvARQs <- sequence 
	                [  recvWithARQ (bridge_frames o)
                        | o <- outputs
                        ]


--	-- Send first volley, to start the service
--	sendARQ (Packet $ BS.pack [])

        return (sendARQs,recvARQs)

-- | 'bridge_byte_driver' creates a driver from the 'Bridge Byte' abstraction,
-- for example an RS-232.
bridge_byte_driver :: String -> Limit -> ([String] -> IO (Bridge Byte)) -> IO ()
bridge_byte_driver name limit bridge_fn = bridge_frame_driver name limit $ \ args -> do
	bridge_byte <- bridge_fn args
	-- and return the higher level frame protocol	
        frameProtocol bridge_byte

