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
import Network.Socket.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.MVar

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
       		addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
		print addrinfos
       		let serveraddr = head addrinfos
       		sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
		print sock
{-
		hd <- socketToHandle sock ReadWriteMode
		hPutStrLn "Hello\n" hd
		hFlush hd
		print hd
-}		

	 	connect sock (addrAddress serveraddr)

		sp <- socketPort sock
		print sp

--		sendAll sock $ encode (0x1234 :: Word32)
--		sendAll sock $ encode (0x1234 :: Word32)
		
		
--		listen sock 1

		sendVar <- newEmptyMVar :: IO (MVar Packet)
		recvVar <- newEmptyMVar :: IO (MVar Packet)

		-- This connects the "sendVar" MVar with the outgoing connection.
		-- putting something onto this MVar will send it over the connection.
		forkIO $ loop $ do
			packet <- takeMVar sendVar
			sendAll sock $ encode packet

		forkIO $ loop $ do
			bs <- recv sock (fromIntegral maxPacketSize)
			print bs
			putMVar recvVar (decode bs)

		putMVar sendVar (Init 0x1234 1 1)
		print "waiting"
		p <- takeMVar recvVar
		print "done"
		print p
		print "waiting"
		putMVar sendVar (Ack 893 127)
		print "waiting"

{-
		repeatOnTimeout 0.1 $ do
			sendPacket sock (Init 0x1234 1 1)
			n <- recieveAck sock 0x1234
-}
--		repeatOnTimeout 0.1 $ do
--			sendPacket sock (Data 0x1234 1 0 (BS.pack "Hello, World")
--			recieveAck sock 0x1234
			

		print "DONE"
{-
		sendstr  sock (addrAddress serveraddr) $ take 2000 (cycle "Hello, World\n")
-}
{-
		sendstr  sock (addrAddress serveraddr) "Hello, World\n"
		sendstr  sock (addrAddress serveraddr) "Hello, World\n"
		sendstr  sock (addrAddress serveraddr) "Hello, World\n"
-}
		return ()
   	  _ -> error $ "Bad options to remote bridge"


loop :: IO () -> IO ()
loop m = do m ; loop m

{-

repeatOnTimeout :: Float -> IO () -> IO ()
repeatOnTimeout 


sendPacket :: Socket -> Packet -> IO ()
sendPacket sock packet = do
	sendAll sock msg

--	if left /= 0 then error "problem with size or transmission of packet" else return ()
  where
	msg = encode packet
-}

{- Hello Packet


4 bytes, says hello, requests reboot of remote service.

-}
{-
sendstr :: Socket -> SockAddr -> String -> IO ()
sendstr sock addr []   = return ()
sendstr sock addr omsg = do sent <- sendTo sock omsg addr
			    print sent
			    sendstr sock addr (genericDrop sent omsg)
-}
loop m = do m ; loop m

{-
openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port progname =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.

       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle sock progname (addrAddress serveraddr)

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    sendstr sendmsg
    where code = makeCode fac pri
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
                    ": " ++ msg

          -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)
          
closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)
-}