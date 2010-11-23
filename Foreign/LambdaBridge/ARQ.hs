-- Implementation of ARQ; http://en.wikipedia.org/wiki/Automatic_repeat_request
-- Can be used to put a lightweight reliable link on top of an unreliable packet, like UDP, or a unreliable bytestream like RS232.

-- This implements "Go-Back-N ARQ".

module Foreign.LambdaBridge.ARQ where

import Data.Word
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
		
-- 2 bit session id
type SessionId = Word16
type PacketId  = Word16
type ChannelId = Word8		-- one of 256 channels (=> 256 FIFOs), zero is always supported

-- | The init packet gets the device ready for servin packets. 
-- Also, the init packet is considered packet zero, a first packet.

data Packet 
	= Init 	SessionId	-- ^ Sending the same Init session id twice 
				-- does nothing. Sending a new session id
				-- resets the service. 
				-- (This is to allow the possibility of 
				-- duplicate packed being transmitted)


		ChannelId	-- ^ Max send channel	
		ChannelId	-- ^ and Max recv channel
				-- These are fixed in the remote device; this just checks for sanity
				-- that the expectations are the same.



-- | The data packet. It is *always* okay to fragment a packet, as long as the packets do not appear out of order.

	| Data  SessionId	-- ^ The session id, a uniq number for the connection session
		PacketId	-- ^ the number of the packet
		ChannelId	-- ^ which FIFO
		BS.ByteString	-- ^ the data (strict bytestring, not lazy)

	| Ack	SessionId	-- ^ The session id, a uniq number for the connection session
		PacketId	-- ^ packet I just got

		deriving (Show)

instance Binary Packet where
	put (Init sessionid s_max r_max) = do
		put (0xfe :: Word8)
		put sessionid
		put s_max
		put r_max
	put (Ack sessId packId) = do
		put (0xa0 :: Word8)
		put sessId
		put packId

	get = do tag <- get
		 case tag :: Word8 of
		    0xfe -> do sessId <- get
			       sendCh <- get
			       recvCh <- get
			       return $ Init sessId sendCh recvCh
		    0xa0 -> do sessId <- get
			       packId <- get
			       return $ Ack sessId packId
			
	
