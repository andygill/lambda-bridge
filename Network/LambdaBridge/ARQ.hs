{-# LANGUAGE RankNTypes #-}
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
import System.Random
import System.IO
import Data.Time.Clock
import Data.Default
import Control.Monad


import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout

-- TMP
import Network.LambdaBridge.Frame
import Data.Char


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

sendWaitingForAck :: (IO () -> IO (Maybe ())) -> PacketId -> IO ByteString -> IO Ack -> (Data -> IO ()) -> MVar (Maybe Double) -> MVar Int -> IO ()
sendWaitingForAck timeout p_id takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	bs <- takeToSendVar
	sendWaitingForAck' timeout p_id (Data p_id bs) takeToSendVar takeAck putSend timeoutVar timeoutTime

sendWaitingForAck' :: (IO () -> IO (Maybe ())) -> PacketId -> Data -> IO ByteString -> IO Ack -> (Data -> IO ()) -> MVar (Maybe Double) -> MVar Int -> IO ()
sendWaitingForAck' timeout p_no p takeToSendVar takeAck putSend timeoutVar timeoutTime = do
	-- send the packet
	print "send the packet"
	putSend p
	print "sent"
	
	tm  <- readMVar timeoutTime
	tm0 <- getCurrentTime
	-- wait for the awk (ignoring other awks)
	print "waiting"
	getAck <- timeout $ let
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
			 sendWaitingForAck' timeout p_no p takeToSendVar takeAck putSend timeoutVar timeoutTime
		-- done, move on please
	   Just () -> do tm1 <- getCurrentTime
		         sendWaitingForAck timeout (p_no + 1) takeToSendVar takeAck putSend timeoutVar timeoutTime


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



sendWithARQ :: Bridge Frame -> Timeout Double -> IO (BS.ByteString -> IO ())
sendWithARQ bridge tm = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)
	timeoutVar  <- newEmptyMVar :: IO (MVar (Maybe Double))
	timeoutTime <- handleTimeouts timeoutVar

	timeout' <- timeout tm
	
	-- This waits for awk's when sending packets
	forkIO $ sendWaitingForAck 
				timeout'
				0
				(takeMVar toSendVar)
				(liftM fromFrame $ fromBridge bridge)
				(toBridge bridge . toFrame)
				timeoutVar
				timeoutTime

	return $ putMVar toSendVar 

recvWithARQ :: Bridge Frame -> IO (IO BS.ByteString)
recvWithARQ bridge = do
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)

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

	let u = def { loseU = 0.01, dupU = 0.001, mangleU = 0.005, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte_lhs <- unreliableBridge u def bridge_byte_lhs
	bridge_byte_rhs <- unreliableBridge u def bridge_byte_rhs

--	bridge_byte_lhs <- debugBridge "bridge_byte_lhs" bridge_byte_lhs
--	bridge_byte_rhs <- debugBridge "bridge_byte_rhs" bridge_byte_rhs
	
	bridge_frame_lhs <- frameProtocol 0.01 bridge_byte_lhs
	bridge_frame_rhs <- frameProtocol 0.01 bridge_byte_rhs

	send <- sendWithARQ bridge_frame_lhs $ Timeout 1 $ \ t o -> 
			case o of
			   Nothing -> min (t * 2) 10
			   Just a  -> (a * 4 + t) / 2
	
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

