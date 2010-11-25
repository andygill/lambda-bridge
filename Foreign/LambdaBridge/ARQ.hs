-- Implementation of ARQ; http://en.wikipedia.org/wiki/Automatic_repeat_request
-- Can be used to put a lightweight reliable link on top of an unreliable packet, like UDP, or a unreliable bytestream like RS232.

-- This implements "Go-Back-N ARQ".

module Foreign.LambdaBridge.ARQ 
	( SessionId
	, PacketId
	, ChannelId
	, Data(..)
	, Ack(..)
	, Packet(..)
	, ARQ_Options(..)
	, ARQ_Protocol(..)
	, arqProtocol
	) where

import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import System.Timeout 
import System.Random
import System.IO
import Data.Time.Clock

-- 2 bit session id
type SessionId = Word16
type PacketId  = Word16
type ChannelId = Word8		-- ^ one of 255 channels (=> 256 FIFOs), 0 is controller, 1 is the main fifo.

-- | The data packet. It is always okay to fragment a packet, as long as the packets do not appear out of order.
-- exception: The 0 (control) channel should not be fragmented.

data Data = Data
		PacketId	-- ^ the number of the packet
		ChannelId	-- ^ which FIFO
		BS.ByteString	-- ^ the data (strict bytestring, not lazy)
	deriving (Show)
	
data Ack = Ack
		PacketId	-- ^ packet I just got
	deriving (Show)

data Packet
	= DataPacket 	Data
	| AckPacket 	Ack
	| BadPacket		-- use to denote a scrambled packet
	deriving (Show)

instance Binary Packet where
	put (DataPacket (Data packId chanId bs)) = do
		putWord8 0x80
		put packId
		put chanId
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	put (AckPacket (Ack packId)) = do
		putWord8 0xa0 
		put packId

	get = do tag <- getWord8
		 case tag :: Word8 of
		    0x80 -> do packId <- get
			       chanId <- get
			       sz <- get :: Get.Get Word16
			       bs <- Get.getByteString (fromIntegral sz)
			       return $ DataPacket $ Data packId chanId bs
		    0xa0 -> do packId <- get
			       return $ AckPacket $ Ack packId
		    _ -> return $ BadPacket


sendWaitingForAck :: PacketId -> IO (ChannelId,BS.ByteString) -> IO Ack -> (Packet -> IO ()) -> MVar (Maybe Double) -> MVar Int -> IO ()
sendWaitingForAck p_id takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	(chan,bs) <- takeToSendVar
	sendWaitingForAck' p_id (Data p_id chan bs) takeToSendVar takeAck putSend timeoutVar timeoutTime

sendWaitingForAck' p_no p takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	-- send the packet
	putSend (DataPacket p)

	tm  <- readMVar timeoutTime
	tm0 <- getCurrentTime
	-- wait for the awk (ignoring other awks)
	getAck <- timeout tm $ let
		loop = do
		   Ack ack <- takeAck
		   if ack == p_no
			then return ()
			else loop
	    in loop
	-- be careful, http://en.wikipedia.org/wiki/Sorcerer%27s_Apprentice_Syndrome
	case getAck of
		-- timeout happened, try send packet again
	   Nothing -> do putMVar timeoutVar Nothing
			 sendWaitingForAck' p_no p takeToSendVar takeAck putSend timeoutVar timeoutTime
		-- done, move on please
	   Just () -> do tm1 <- getCurrentTime
			 putMVar timeoutVar $ Just (realToFrac $ tm1 `diffUTCTime` tm0) 
		         sendWaitingForAck (p_no + 1) takeToSendVar takeAck putSend timeoutVar timeoutTime


handleTimeouts :: MVar (Maybe Double) -> IO (MVar Int)
handleTimeouts timings = do
	let firstTime = 1 :: Double	-- 1 second max latency (0.5 each direction)
	var <- newEmptyMVar
	let loop avr = do
		tryTakeMVar var
		putMVar var (floor (avr * 1000 * 1000))	-- av is the best guess so far for a good timeout
		v <- takeMVar timings
{-
		case v of
		  Nothing -> print avr
		  Just {} -> return ()
-}
		case v of
			-- Timeout happend, blame the timeout, double the timeout
		   Nothing -> loop (min (avr * 2) firstTime)
			-- Wait at least the average of 4 times average length of this packet and the old timeout.
			-- This stops one single fast (mistaken) round trip time from messing things up
		   Just t  -> loop ((t * 4 + avr) / 2)
	forkIO $ loop firstTime
	return var


recvReplyWithAck :: PacketId -> IO Data -> (Packet -> IO ()) -> ((ChannelId,BS.ByteString) -> IO ()) -> IO ()
recvReplyWithAck n takeRecvVar putSendVar putHaveRecvVar = do
		p@(Data packId chanId bs) <- takeRecvVar
		putSendVar (AckPacket $ Ack packId)	-- *always* send an awk
		if packId == n then do
			putHaveRecvVar (chanId,bs)
			recvReplyWithAck (n + 1) takeRecvVar putSendVar putHaveRecvVar
		     -- Otherwise, just ignore the (out of order) packet
		     -- and eventually the client will resend (because there is no Ack)
		     -- Todo: consider Nack		
		  else do recvReplyWithAck n takeRecvVar putSendVar putHaveRecvVar

sendBits :: [Bool] -> IO Packet -> (BS.ByteString -> IO ()) -> IO ()
sendBits (True:errs) takeSendVar putSockVar = do
	packet <- takeSendVar
	sendBits errs takeSendVar putSockVar
sendBits (False:errs) takeSendVar putSockVar = do
	packet <- takeSendVar
	let bs = BS.concat $ LBS.toChunks $ encode packet
	putSockVar bs
	sendBits errs takeSendVar putSockVar

recvBits :: [Bool] -> IO BS.ByteString -> (Data -> IO ()) -> (Ack -> IO ()) -> IO ()
recvBits (True:errs) takeRecv putRecvVar putAckVar = do
	bs <- takeRecv
	recvBits errs takeRecv putRecvVar putAckVar
recvBits (False:errs) takeRecv putRecvVar putAckVar = do
	bs <- takeRecv
	case decode (LBS.fromChunks [bs]) of
	  	DataPacket dat -> do putRecvVar dat
	  	AckPacket ack  -> do putAckVar ack
	  	_ -> do return ()	-- ignore this packet
	recvBits errs takeRecv putRecvVar putAckVar 

data ARQ_Options = ARQ_Options
	{ toSocket 	  :: BS.ByteString -> IO ()	-- 
	, fromSocket 	  :: IO BS.ByteString
	, transmitFailure :: Maybe Float
	, receiveFailure  :: Maybe Float
	}

data ARQ_Protocol = ARQ_Protocol
	{ sendByteString :: (ChannelId,BS.ByteString) -> IO ()
	, recvByteString :: IO (ChannelId,BS.ByteString)
	}
	
arqProtocol :: ARQ_Options -> IO ARQ_Protocol
arqProtocol opts = do
	seed <- getStdGen
	let (s1,s2) = split seed
	let sendFailures = case transmitFailure opts of
		 	    Nothing -> repeat False
			    Just f -> [ n < f | n <- randoms s1 ]
	let recvFailures = case receiveFailure opts of
		 	    Nothing -> repeat False
			    Just f -> [ n < f | n <- randoms s2 ]
	toSendVar <- newEmptyMVar :: IO (MVar (ChannelId,BS.ByteString))
	sendVar <- newEmptyMVar :: IO (MVar Packet)
	ackVar  <- newEmptyMVar :: IO (MVar Ack)
	recvVar <- newEmptyMVar :: IO (MVar Data)
	haveRecvVar <- newEmptyMVar :: IO (MVar (ChannelId,BS.ByteString))
	timeoutVar <- newEmptyMVar :: IO (MVar (Maybe Double))
	timeoutTime <- handleTimeouts timeoutVar

		-- This waits for awk's when sending packets
	forkIO $  sendWaitingForAck 0
				(takeMVar toSendVar)
				(takeMVar ackVar)
				(putMVar sendVar)
				timeoutVar
				timeoutTime

		-- This connects the "sendVar" MVar with the outgoing connection.
	forkIO $ sendBits sendFailures (takeMVar sendVar) (toSocket opts)
		

		-- This send bits over the connection
	forkIO $ recvBits recvFailures
			     (fromSocket opts)
			     (putMVar recvVar)
			     (putMVar ackVar)

		-- This sends acks
	forkIO $ recvReplyWithAck 0 
				(takeMVar recvVar) 
				(putMVar sendVar) 
				(putMVar haveRecvVar)

	return $ ARQ_Protocol
	  { sendByteString = \ (chanId,bs) -> putMVar toSendVar (chanId,bs)
	  , recvByteString = takeMVar haveRecvVar
	  }

