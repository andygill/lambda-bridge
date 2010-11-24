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
	let rnd = NoRandomErrors

	let port = "9237"

	-- Create a socket
    	sock <- socket AF_INET Datagram defaultProtocol

	-- Bind it to the address we're listening to
	bindSocket sock $ SockAddrInet (fromIntegral (read port :: Integer)) iNADDR_ANY

	print "pre list"
--	listen sock 1
	print "post list"
	
{-
	(sock2,addr)  <- accept sock
	print sock2
	print addr
-}


	-- semiphore for init for channel
	connVar <- newMVar () :: IO (MVar ())

	-- this is the LHS

	-- The 
	recvVar <- newEmptyMVar :: IO (MVar Data)
	ackVar <- newEmptyMVar :: IO (MVar Ack)

	haveRecvVar <- newEmptyMVar :: IO (MVar Data)
	sendVar <- newEmptyMVar :: IO (MVar Packet)
	loopback <- newEmptyMVar  :: IO (MVar BS.ByteString)
	toSendVar <- newEmptyMVar :: IO (MVar Data)


	forkIOs 
	  [ loop () $ \ () -> do
		pn <- socketPort sock
		print ("pn",pn)	
		(msg,from) <- recvFrom sock 2000

		print msg
		-- Connect *once*
		conn <- tryTakeMVar connVar
		case conn of
		  Just f -> connect sock from
		  Nothing -> return ()

		case decode (LBS.fromChunks [msg]) of
		   DataPacket dat -> do putMVar recvVar dat
		   AckPacket ack  -> do putMVar ackVar ack
		   _ -> do hPutStrLn stderr "badly formed packet received (ignoring)"
			   return ()
		
	  , loop 0 $ \ n -> do
		p@(Data packId chanId bs) <- takeMVar recvVar
		putMVar sendVar (AckPacket $ Ack packId)	-- *always* send an awk
		if packId == n then do
			print (chanId,bs)
			putMVar haveRecvVar p
			return (n+1)
		     -- Otherwise, just ignore the (out of order) packet
		     -- and eventually the client will resend (because there is no Ack)
		     -- Todo: consider Nack
		  else do print ("OUT OF ORDER PACKET S",packId,n)
			  return n

	  , loop () $ \ () -> do
		(Data _ chanId bs) <- takeMVar haveRecvVar
			-- Now, insert into pipe, depending on the chan Id
		case chanId of
			0 -> print ("CONTROL says ",bs)
			1 -> putMVar loopback bs
		return ()

	  , loop 0 $ \ n -> do
		back <- takeMVar loopback
		putMVar toSendVar (Data n 1 back)
		return (n+1)

	  , loop Nothing $ \ optPacket -> do
			-- TODO: include awks here
			-- Can only be data (or Init, which is a first data packet)
			print "waiting for toSendVar"
			p <- case optPacket of
				Nothing -> takeMVar toSendVar 	-- really to 
				Just p' -> return p'
			let p_no = case p of
			   	     Data p_no _ _ -> p_no
			-- send the packet
			putMVar sendVar (DataPacket p)

			-- TODO: add timeout
			getAck <- timeout 100000 $ let
				loop = do
				   (Ack ack) <- takeMVar ackVar
				   if ack == p_no then return () else loop in loop
				

			print getAck
			case getAck of
				-- timeout happened, try send packet again
			   Nothing -> return (Just p)
				-- done, move on please
			   Just () -> return Nothing
 

	-- Must send *after* receiving first packet (or the channel will not be bound)
	  , sendBits rnd (takeMVar sendVar) (sendAll sock)
	  ]


{-
--	(sock2,addr)  <- accept sock

	forkIO $ loop () $ \ () -> do
		pn <- socketPort sock
		print ("pn",pn)	
		(msg,from) <- recvFrom sock 2000

		-- Connect *once*
		conn <- tryTakeMVar connVar
		case conn of
		  Just f -> connect sock from
		  Nothing -> return ()

		putMVar recvVar (decode (C8.fromChunks [msg]))
		
		return ()

		
	forkIO $ loop 0 $ \ n -> do
		p <- takeMVar recvVar
		print p
		case p of
		  Init a b -> do
			print ("Init",p)
			putMVar sendVar (Ack 0)
			return 0	-- restart!

		  Data packId _ bs -> do
			putMVar sendVar (Ack packId)
			print bs
			putMVar loopback bs
			return packId

		  Ack {} -> do
			return ()
			return n


	loop Nothing $ \ optPacket -> do
			p <- case optPacket of
				Nothing -> takeMVar toSendVar
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
			   Just p_no' | p_no' == p_no -> return Nothing 	-- done, move one
			   other -> error $ show ("BAD BAD BAD",other,p_no)

	return ()
--	protocol recvVar sendVar
		
-}
	return ()

forkIOs :: [IO ()] -> IO ()
forkIOs [] = return ()
forkIOs [x] = x
forkIOs (x:xs) = do { forkIO x ; forkIOs xs }
	
loop :: a -> (a -> IO a) -> IO ()
loop a m = do 
	a' <- m a
	loop a' m 

