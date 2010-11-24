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

import Foreign.LambdaBridge.Service
import Foreign.LambdaBridge.ARQ

-- This Bridge service uses UDP to send and recieve requests,
-- over (default) port 9237.

-- How much to listen for

maxPacketSize :: Int
maxPacketSize = 2000

data SessionHandle = SessionHandle Socket SockAddr

main = bridge_service $ \ args sends recvs -> do
	hPutStrLn stderr "Remote Service:"
	case (args,sends,recvs) of
	  (remote_cmd:hostname:port:rest,[s],[r]) -> do
		print $ "got" ++ show (hostname,port,rest,s,r)
		let rnd = NoRandomErrors

 		addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    (Just hostname) (Just port)
		let serveraddr = head addrinfos

       		sock <- socket AF_INET Datagram defaultProtocol

	 	connect sock $ addrAddress serveraddr

		protocol <- arqProtocol $ ARQ_Options
			{ toSocket 	  = sendAll sock
			, fromSocket 	  = recv sock (fromIntegral maxPacketSize)
			, transmitFailure = Nothing
			, receiveFailure  = Nothing
			}

		forkIO $ 
		   let loop = do
			hWaitForInput s (-1)
			bs <- BS.hGetNonBlocking s 100	-- 100???
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
