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
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char as Char
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import Control.Concurrent.MVar

import Prelude hiding (getContents)

import Foreign.LambdaBridge.ARQ

maxPacketSize :: Int
maxPacketSize = 2000

main = do
	let port = "9237"
 	addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    (Just "127.0.0.1")  (Just port)
	let serveraddr = head addrinfos

	print serveraddr

	-- Create a socket
	sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

	-- Bind it to the address we're listening to
	bindSocket sock (addrAddress serveraddr)

	print "pre list"
--	listen sock 1
	print "post list"
	
{-
	(sock2,addr)  <- accept sock
	print sock2
	print addr
-}

	pn <- socketPort sock
	print pn
	
	recvVar <- newEmptyMVar :: IO (MVar Packet)
	sendVar <- newEmptyMVar :: IO (MVar Packet)
	connVar <- newMVar () :: IO (MVar ())

--	(sock2,addr)  <- accept sock

	forkIO $ loop $ do
		pn <- socketPort sock
		print ("pn",pn)	
		(msg,from) <- recvFrom sock 2000

		-- Connect *once*
		conn <- tryTakeMVar connVar
		case conn of
		  Just f -> connect sock from
		  Nothing -> return ()

		putMVar recvVar (decode (C8.fromChunks [msg]))

	-- Must send *after* receiving first packet
	forkIO $ loop $ do
		packet <- takeMVar sendVar
		sendAll sock $ BS.concat $ C8.toChunks $ encode packet

	protocol recvVar sendVar

protocol :: MVar Packet -> MVar Packet -> IO ()
protocol recvVar sendVar = do
	p <- takeMVar recvVar
	print p
	putMVar sendVar (Ack 327 354)
	protocol recvVar sendVar

	
loop :: IO () -> IO ()
loop m = do m ; loop m

lazyToStrict :: LBS.ByteString -> BS.ByteString
lazyToStrict = undefined