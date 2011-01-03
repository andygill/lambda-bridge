-- | Implementation of ARQ; http://en.wikipedia.org/wiki/Automatic_repeat_request "Stop-and-wait ARQ".
-- Can be used to put a lightweight reliable link on top of an unreliable packet system, like UDP,
-- or a unreliable bytestream system like RS232.

module Network.LambdaBridge.ARQ 
{-	( SessionId
	, PacketId
	, BridgePort
	, Data(..)
	, Ack(..)
	, Packet(..)
	, arqProtocol
	) where -}
		where

import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import System.Timeout 
import System.Random
import System.IO
import Data.Time.Clock
import Data.Default
import Control.Monad


import Network.LambdaBridge.Bridge

-- TMP
import Network.LambdaBridge.Frame
import Data.Char


-- timeout a m = do { r <- m ; return $ Just r }


-- 2 bit session id
type SessionId = Word16
type PacketId  = Word16

-- | The data packet. It is always okay to fragment a packet, as long as the packets do not appear out of order.
-- exception: The 0 (control) channel should not be fragmented.

data Data = Data
		PacketId	-- the number of the packet
		BS.ByteString	-- the data (strict bytestring, not lazy)
	deriving (Show)
	
instance Binary Data where
	put (Data packId bs) = do
		put packId
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	get = do packId <- get
	         sz <- get :: Get.Get Word16
	         bs <- Get.getByteString (fromIntegral sz)
	         return $ Data packId bs

data Ack = Ack
		PacketId	-- ^ packet I just got
	deriving (Show)
	
instance Binary Ack where
	put (Ack packId) = do
		put packId

	get = do packId <- get
		 return $ Ack packId

data Packet
	= DataPacket 	Data
	| AckPacket 	Ack
	| BadPacket		-- use to denote a scrambled packet
	deriving (Show)

instance Binary Packet where
	put (DataPacket (Data packId bs)) = do
		putWord8 0x80
		put packId
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	put (AckPacket (Ack packId)) = do
		putWord8 0xa0 
		put packId

	get = do tag <- getWord8
		 case tag :: Word8 of
		    0x80 -> do packId <- get
			       sz <- get :: Get.Get Word16
			       bs <- Get.getByteString (fromIntegral sz)
			       return $ DataPacket $ Data packId bs
		    0xa0 -> do packId <- get
			       return $ AckPacket $ Ack packId
		    _ -> return $ BadPacket


sendWaitingForAck :: PacketId -> IO ByteString -> IO Ack -> (Data -> IO ()) -> MVar (Maybe Double) -> MVar Int -> IO ()
sendWaitingForAck p_id takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	bs <- takeToSendVar
	sendWaitingForAck' p_id (Data p_id bs) takeToSendVar takeAck putSend timeoutVar timeoutTime

sendWaitingForAck' p_no p takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	-- send the packet
	print "send the packet"
	putSend p
	print "sent"
	
	tm  <- readMVar timeoutTime
	tm0 <- getCurrentTime
	-- wait for the awk (ignoring other awks)
	print "waiting"
	getAck <- timeout tm $ let
		loop = do
		   print "waiting..."
		   Ack ack <- takeAck
		   print $ "found " ++ show ack
		   if ack == p_no
			then return ()
			else loop
	    in loop

	print $ "waited" ++ show getAck

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
	let firstTime = 1 / 1000 :: Double	-- 1 second max latency (0.5 each direction)
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

recvReplyWithAck :: PacketId -> IO Data -> (Ack -> IO ()) -> (ByteString -> IO ()) -> IO ()
recvReplyWithAck n takeRecvVar putAckVar putHaveRecvVar = do
		print "recvReplyWithAck"
		Data packId bs <- takeRecvVar
		print $ "got packet, sending ack " ++ show packId
		putAckVar (Ack packId)	--   *always* send an awk
		print "sent awk"
		if packId == n then do
			putHaveRecvVar bs
			recvReplyWithAck (n + 1) takeRecvVar putAckVar putHaveRecvVar
		     -- Otherwise, just ignore the (out of order) packet
		     -- and eventually the client will resend (because there is no Ack)
		     -- Todo: consider Nack		
		  else do recvReplyWithAck n takeRecvVar putAckVar putHaveRecvVar

{-
sendBits :: IO Packet -> (Frame -> IO ()) -> IO ()
sendBits takeSendVar putSockVar = do
	packet <- takeSendVar
	let bs = BS.concat $ LBS.toChunks $ encode packet
	putSockVar (Frame bs)
	sendBits takeSendVar putSockVar

recvBits :: IO Frame -> (Data -> IO ()) -> (Ack -> IO ()) -> IO ()
recvBits takeRecv putRecvVar putAckVar = do
	Frame bs <- takeRecv
	case decode (LBS.fromChunks [bs]) of
	  	DataPacket dat -> do putRecvVar dat
	  	AckPacket ack  -> do putAckVar ack
	  	_ -> do return ()	-- ignore this packet
	recvBits takeRecv putRecvVar putAckVar 
-}
{-

arqProtocol :: Bridge () Frame -> IO (Bridge BridgePort Link)
arqProtocol opts = do
	toSendVar <- newEmptyMVar :: IO (MVar (BridgePort,Link))
	sendVar <- newEmptyMVar :: IO (MVar Packet)
	ackVar  <- newEmptyMVar :: IO (MVar Ack)
	recvVar <- newEmptyMVar :: IO (MVar Data)
	haveRecvVar <- newEmptyMVar :: IO (MVar (BridgePort,Link))
	timeoutVar <- newEmptyMVar :: IO (MVar (Maybe Double))
	timeoutTime <- handleTimeouts timeoutVar

		-- This waits for awk's when sending packets


		-- This connects the "sendVar" MVar with the outgoing connection.
	forkIO $ sendBits (takeMVar sendVar) (toBridge opts ())


		-- This receives bits over the connection
	forkIO $ recvBits    (fromBridge opts ())
			     (putMVar recvVar)
			     (putMVar ackVar)


		-- This sends acks
	forkIO $ recvReplyWithAck 0 
				(takeMVar recvVar) 
				(putMVar sendVar) 
				(putMVar haveRecvVar)


	return $ Bridge
	  { toBridge = \ port link -> putMVar toSendVar (port,link)
			-- TODO: add chunking to packet size
	  , fromBridge = \ port -> do
				(port,link) <- takeMVar haveRecvVar
				return link
	  }
-}


sendWithARQ :: Bridge Frame -> IO (BS.ByteString -> IO ())
sendWithARQ bridge = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)
	sendVar     <- newEmptyMVar :: IO (MVar Packet)
	ackVar      <- newEmptyMVar :: IO (MVar Ack)
	timeoutVar  <- newEmptyMVar :: IO (MVar (Maybe Double))
	timeoutTime <- handleTimeouts timeoutVar
	
	-- This waits for awk's when sending packets
	forkIO $ sendWaitingForAck 0
				(takeMVar toSendVar)
				(liftM fromFrame $ fromBridge bridge)
				(toBridge bridge . toFrame)
				timeoutVar
				timeoutTime

	return $ putMVar toSendVar 

recvWithARQ :: Bridge Frame -> IO (IO BS.ByteString)
recvWithARQ bridge = do
	recvVar     <- newEmptyMVar :: IO (MVar Data)
	sendVar     <- newEmptyMVar :: IO (MVar Packet)
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)
	ackVar      <- newEmptyMVar :: IO (MVar Ack)		-- no longer needed

		-- This receives bits over the connection
--	forkIO $ recvBits    (fromBridge bridge)
--			     (putMVar recvVar)
--			     (putMVar ackVar)

	-- This sends acks
	forkIO $ recvReplyWithAck 0 
				(liftM fromFrame $ fromBridge bridge)
				(toBridge bridge . toFrame)
				(putMVar haveRecvVar)

	return $ takeMVar haveRecvVar

-- Set up an end to end to test things.

	
main :: IO ()
main = do
	bridge_byte_lhs0 <- stubBridge $ [] -- cycle [Byte n | n <- [0..255]]

	(bridge_byte_lhs,bridge_byte_rhs) <- pipeBridge :: IO (Bridge Byte, Bridge Byte)	

	let u = def { loseU = 0.05, dupU = 0.005, mangleU = 0.005, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte_lhs <- unreliableBridge u def bridge_byte_lhs
	bridge_byte_rhs <- unreliableBridge u def bridge_byte_rhs

--	bridge_byte_lhs <- debugBridge "bridge_byte_lhs" bridge_byte_lhs
--	bridge_byte_rhs <- debugBridge "bridge_byte_rhs" bridge_byte_rhs
	
	bridge_frame_lhs <- frameProtocol 0.01 bridge_byte_lhs
	bridge_frame_rhs <- frameProtocol 0.01 bridge_byte_rhs

	send <- sendWithARQ bridge_frame_lhs
	recv <- recvWithARQ bridge_frame_rhs

	forkIO $ let loop n = do
			send (toStr $ show (n :: Int))
			loop (succ n)
		 in loop 0

	forkIO $ let loop = do
			msg <- recv
			print msg
			loop
		 in loop

	threadDelay (1000 * 1000 * 1000)

	return ()
   where	
	toStr :: String -> BS.ByteString
	toStr = BS.pack . map (fromIntegral . ord)

	
{-
	bridge_byte0 <- loopbackBridge
	
	bridge_byte1 <- debugBridge bridge_byte0
	let u = def { loseU = 0.01, dupU = 0.01, mangleU = 0.01, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte2 <- unreliableBridge def def bridge_byte0

--	sequence_ [ toBridge bridge_byte' x | x <- [0..255]]

	bridge_frame <- frameProtocol 0.01 bridge_byte2

	forkIO $ do
		sequence [ toBridge bridge_frame $ Frame (toStr $ "Frame: " ++ show i ++ " " ++ [' '..'~'] ++ [chr 0xf1])
			 | i <- [1..]
			 ]
		return ()
		
	sequence [ do
		frame <- fromBridge bridge_frame
		print frame
			| _ <- [1..1000]]

	return ()
-}