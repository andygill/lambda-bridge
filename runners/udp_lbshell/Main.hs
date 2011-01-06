module Main where
	
import System.IO
import Data.Char (toUpper)
import Data.Bits
import Network.BSD
import Data.List
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Char as Char
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import System.Timeout 
import System.Random
import System.Environment

import Prelude hiding (getContents)

import Network.LambdaBridge
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.Multiplex
import Network.LambdaBridge.ARQ

-- ./Main 9237 <hostname>

main = do 
	args <- getArgs
	putStrLn $ "Seting up to a udp-shell lambda bridge using " ++ show args 
	let (port:style:_) = args
	
	args <- return $ drop 2 args

	let sockType = case style of
		  "UDP" -> Datagram
		  "TCP" -> Stream
		  _     -> error "is this UDP?"

	-- Create a socket
	sock <- socket AF_INET sockType defaultProtocol

	-- Bind it to the address we're listening to
	bindSocket sock $ SockAddrInet (fromIntegral (read port :: Integer)) iNADDR_ANY

	-- semaphore for init for channel
	connVar <- newMVar () :: IO (MVar ())
	
	sock <- case sockType of
		  Datagram -> return sock
		  Stream -> do 
			-- do not connect the socket
			tryTakeMVar connVar
			listen sock 1
			(sock2,_) <- accept sock
			return sock2


	let bridge_frame = Bridge
		{ toBridge = \ (Frame bs) -> sendAll sock bs
		, fromBridge = do
			(bs,from) <- recvFrom sock 2048
			-- Connect *once*, if Datagram
			conn <- tryTakeMVar connVar
			case conn of
			  Just f -> connect sock from
			  Nothing -> return ()			
			return $ Frame bs
		}

	-- This needs driven by the command line arguments
	bridge_frames <- multiplexBridge [0x33,0x34] bridge_frame

	let bridge_frame_0x33 = bridge_frames 0x33
	let bridge_frame_0x34 = bridge_frames 0x34

	sendARQ <- sendWithARQ bridge_frame_0x34 $ boundLimit 0.3

	recvARQ <- recvWithARQ bridge_frame_0x33

	let loop 10 = return ()
	    loop n = do
		link <- recvARQ
		print link
		sendARQ link
		case link of
		   Link bs | BS.length bs == 0 -> loop (n + 1)
		   _ -> loop n

	loop 0

{-
	-- Send first volley, to start the service
	sendARQ (Link $ BS.pack [])

	return ()

	forever $ do
		link <- recvARQ
		print link
	
-}
{-

	protocol <- arqProtocol $ ARQ_Options
		{ toSocket	  = sendAll sock
		, fromSocket	  = do
			(bs,from) <- recvFrom sock 2048
			-- Connect *once*, if Datagram
			conn <- tryTakeMVar connVar
			case conn of
			  Just f -> connect sock from
			  Nothing -> return ()			
			return bs
		, transmitFailure = Nothing
		, receiveFailure  = Nothing
		}


	putStrLn $ "waiting for remote stream(s)"

	(chanId,bs) <- recvByteString protocol

	putStrLn $ "invoking lambda bridge"

	(send,recv) <- simple_board_connect args

	putStrLn $ "connected."
	
	let loop = do
		(chanId,bs) <- recvByteString protocol
		case chanId of
			0 -> print ("CONTROL:",bs)
		 	1 -> BS.hPutStr send bs
		loop

	forkIO loop

	let loop = do
		hWaitForInput recv (-1)
		bs <- BS.hGetNonBlocking recv 1024
		sendByteString protocol (1,bs)
		loop 

	loop
-}
	return ()