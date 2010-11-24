-- Implementation of ARQ; http://en.wikipedia.org/wiki/Automatic_repeat_request
-- Can be used to put a lightweight reliable link on top of an unreliable packet, like UDP, or a unreliable bytestream like RS232.

-- This implements "Go-Back-N ARQ".

module Foreign.LambdaBridge.ARQ where

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

-- 2 bit session id
type SessionId = Word16
type PacketId  = Word16
type ChannelId = Word8		-- one of 255 channels (=> 256 FIFOs), 0 is controller, 1 is the main fifo.

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


sendWaitingForAck :: IO Data -> IO Ack -> (Packet -> IO ()) -> IO ()
sendWaitingForAck takeToSendVar takeAck putSend = do
	p <- takeToSendVar
	sendWaitingForAck' p takeToSendVar takeAck putSend

sendWaitingForAck' p@(Data p_no _ _) takeToSendVar takeAck putSend = do
	-- send the packet
	putSend (DataPacket p)

	-- wait for the awk (ignoring other awks)
	getAck <- timeout 100000 $ let
		loop = do
		   (Ack ack) <- takeAck
		   if ack == p_no then return () else loop in loop

	print getAck
	case getAck of
		-- timeout happened, try send packet again
	   Nothing -> sendWaitingForAck' p takeToSendVar takeAck putSend
		-- done, move on please
	   Just () -> sendWaitingForAck takeToSendVar takeAck putSend

recvReplyWithAck :: PacketId -> IO Data -> (Packet -> IO ()) -> (Data -> IO ()) -> IO ()
recvReplyWithAck n takeRecvVar putSendVar putHaveRecvVar = do
		p@(Data packId chanId bs) <- takeRecvVar
		putSendVar (AckPacket $ Ack packId)	-- *always* send an awk
		if packId == n then do
			print (chanId,bs)
			putHaveRecvVar p
			recvReplyWithAck (n+1) takeRecvVar putSendVar putHaveRecvVar
		     -- Otherwise, just ignore the (out of order) packet
		     -- and eventually the client will resend (because there is no Ack)
		     -- Todo: consider Nack		
		  else do recvReplyWithAck n takeRecvVar putSendVar putHaveRecvVar

data RandomErrors = NoRandomErrors 
		  | RandomErrors StdGen Float	-- The generator, and how often packets suceed (0.90 is a good number)

sendBits :: RandomErrors -> IO Packet -> (BS.ByteString -> IO ()) -> IO ()
sendBits err takeSendVar putSockVar = do
	packet <- takeSendVar
	let bs = BS.concat $ LBS.toChunks $ encode packet
	case err of
	  NoRandomErrors -> do 
		putSockVar bs
		sendBits err takeSendVar putSockVar
	  RandomErrors g cmp -> do
		let (b,g') = random g
		if (b :: Float) >= cmp then return () else putSockVar bs
		sendBits (RandomErrors g' cmp) takeSendVar putSockVar


recvBits :: RandomErrors -> IO BS.ByteString -> (Data -> IO ()) -> (Ack -> IO ()) -> IO ()
recvBits err takeRecv putRecvVar putAckVar = do
	bs <- takeRecv
	case decode (LBS.fromChunks [bs]) of
	  DataPacket dat -> do putRecvVar dat
	  AckPacket ack  -> do putAckVar ack
	  _ -> do hPutStrLn stderr "badly formed packet received (ignoring)"
		  return ()
	recvBits err takeRecv putRecvVar putAckVar 
