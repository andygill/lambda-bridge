module Main where
	
import System.IO
import Data.Char (toUpper)
import Data.Bits
--import Network.Socket
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

import Prelude hiding (getContents)

import Network.LambdaBridge.Service
import Network.LambdaBridge.ARQ

-- This Bridge service uses UDP to send and recieve requests,
-- over (default) port 9237.

-- How much to listen for

data SessionHandle = SessionHandle Socket SockAddr

main = bridge_service "lb_ubp driver" $ \ args sends recvs -> do return ()
{-
	
	hPutStrLn stderr "Remote Service:"
	case (args,sends,recvs) of
	  (remote_cmd:hostname:port:style:rest,[s],[r]) -> do
		print $ "got" ++ show (hostname,port,rest,s,r)

		let sockType = case style of
			  "UDP" -> Datagram
			  "TCP" -> Stream
			  _     -> error "is this UDP?"

 		addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    (Just hostname) (Just port)
		let serveraddr = head addrinfos

		sock <- socket AF_INET sockType defaultProtocol

	 	connect sock $ addrAddress serveraddr

		protocol <- arqProtocol $ ARQ_Options
			{ toSocket 	  = sendAll sock
			, fromSocket 	  = recv sock 2048
			, transmitFailure = Nothing
			, receiveFailure  = Nothing
			}

		-- Send first volley, to start the service
		sendByteString protocol (0,BS.pack [0xde,0xad,0xbe,0xef])

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