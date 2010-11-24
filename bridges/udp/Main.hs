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

		toSendVar <- newEmptyMVar :: IO (MVar Data)
		sendVar <- newEmptyMVar :: IO (MVar Packet)
		ackVar  <- newEmptyMVar :: IO (MVar Ack)
		recvVar <- newEmptyMVar :: IO (MVar Data)
		haveRecvVar <- newEmptyMVar :: IO (MVar Data)

		-- Send a starting volley
		putMVar toSendVar $ Data 0 0 (BS.pack [0xde,0xad,0xbe,0xef])

		forkIOs 
		  [ loop 1 $ \ n -> do
			hWaitForInput s (-1)
			bs <- BS.hGetNonBlocking s 100
			print ("STDIN",bs)
			putMVar toSendVar (Data n 1 bs)
			return $ n + 1

		-- This waits for awk's when sending packets
		  , sendWaitingForAck 
				(takeMVar toSendVar)
				(takeMVar ackVar)
				(putMVar sendVar)

		-- This connects the "sendVar" MVar with the outgoing connection.
		  , sendBits rnd (takeMVar sendVar) (sendAll sock)
		
		  , recvBits rnd
			     (recv sock  (fromIntegral maxPacketSize))
			     (putMVar recvVar)
			     (putMVar ackVar)

		  , recvReplyWithAck 0 (takeMVar recvVar) (putMVar sendVar) (putMVar haveRecvVar)

		  , loop () $ \ () -> do
			(Data pack chanId bs) <- takeMVar haveRecvVar
			case chanId of
				0 -> print ("CONTROL",bs)
			 	1 -> BS.hPut r bs
			return ()

		  ]

{-


		forkIO $ loop () $ \ () -> do
			bs <- recv sock (fromIntegral maxPacketSize)
			case decode bs of
			  p@(Data {}) -> do putMVar recvVar p
			  Ack p_id   -> do putMVar ackVar p_id
			  _ -> do hPutStrLn stderr "badly formed packet received (ignoring)"
				  return ()


		-- start the connections discussion
		putMVar toSendVar (Init 0 0)

		-- start at packet 1
		forkIO $ loop 1 $ \ n -> do
			putMVar toSendVar (Data n 0 $ BS.pack (map (fromIntegral . Char.ord) $ show $ "line " ++ show n))
			return (n+1)

{-
		forkIO $ loop 0 $ \ p_id -> do
			bs <- takeMVar toSendVar
			return p_id
-}
		forkIO $ loop Nothing $ \ optPacket -> do
			-- TODO: include awks here
			-- Can only be data (or Init, which is a first data packet)
			p <- case optPacket of
				Nothing -> takeMVar toSendVar 	-- really to 
				Just p' -> return p'
			let p_no = case p of
			   	Data p_no _ _ -> p_no
			 	Init {}       -> 0
			  	Ack {}        -> error "Bad Ack in input data stream"
			-- send the packet
			putMVar sendVar p

			-- TODO: add timeout
			ack <- timeout (-1) $ takeMVar ackVar
			print ack
			case ack of
				-- timeout happened, try send packet again
			   Nothing -> return (Just p)
				-- done, move one
			   Just p_no' | p_no' == p_no -> return Nothing 	
			   other -> error $ show ("BAD BAD BAD",other,p_no)





		loop 1 $ \ n -> do
			p <- takeMVar haveRecvVar
			print (n,"FINALLY",p)
			return (n+1)

-}
{-
		putMVar sendVar (Init 1 1)
		print "waiting"
		p <- takeMVar recvVar
		print "done"
		print p
		print "waiting"
		putMVar sendVar (Ack 127)
		print "waiting"
-}

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

forkIOs :: [IO ()] -> IO ()
forkIOs [] = return ()
forkIOs [x] = x
forkIOs (x:xs) = do { forkIO x ; forkIOs xs }

	
loop :: a -> (a -> IO a) -> IO ()
loop a m = do 
	a' <- m a
	loop a' m 

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