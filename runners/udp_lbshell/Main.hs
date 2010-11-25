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
import Control.Concurrent.MVar
import System.Timeout 
import System.Random
import System.Environment

import Prelude hiding (getContents)

import Foreign.LambdaBridge
import Foreign.LambdaBridge.ARQ

main = do
	let port = "9237"

	-- Create a socket
    	sock <- socket AF_INET Datagram defaultProtocol

	-- Bind it to the address we're listening to
	bindSocket sock $ SockAddrInet (fromIntegral (read port :: Integer)) iNADDR_ANY

	-- semaphore for init for channel
	connVar <- newMVar () :: IO (MVar ())

	protocol <- arqProtocol $ ARQ_Options
		{ toSocket 	  = sendAll sock
		, fromSocket 	  = do
			(bs,from) <- recvFrom sock 2048
			-- Connect *once*
			conn <- tryTakeMVar connVar
			case conn of
			  Just f -> connect sock from
			  Nothing -> return ()			
			return bs
		, transmitFailure = Nothing
		, receiveFailure  = Nothing
		}

	args <- getArgs
	putStrLn $ "Seting up to a udp-shell lambda bridge using " ++ show args 
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

