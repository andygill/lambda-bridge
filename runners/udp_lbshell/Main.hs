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

import Foreign.LambdaBridge.ARQ

maxPacketSize :: Int
maxPacketSize = 2000

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
			(bs,from) <- recvFrom sock 2000
			-- Connect *once*
			conn <- tryTakeMVar connVar
			case conn of
			  Just f -> connect sock from
			  Nothing -> return ()			
			return bs
		, transmitFailure = Nothing
		, receiveFailure  = Nothing
		}



	let loop = do
		(chanId,bs) <- recvByteString protocol
		case chanId of
			0 -> print ("CONTROL:",bs)
		 	1 -> sendByteString protocol (1,bs)
		loop

	loop
