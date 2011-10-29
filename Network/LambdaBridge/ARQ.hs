{-# LANGUAGE RankNTypes #-}
{- | Implementation of ARQ; <http://en.wikipedia.org/wiki/Automatic_repeat_request> ''Stop-and-wait ARQ''.
   Can be used to put a lightweight reliable link on top of an unreliable packet system, like UDP,
   or a unreliable bytestream system like RS232.

The Frame format is trivial, a byte header, then payload.

> <-byte-><-   n-bytes     ->
> +------+------------------+
> | Info |    ... DATA .... |
> +------+------------------+

  There are two types of packets, data packets and ack packets.

  -  Data packets (in bytes)

> <-byte-><-  2 bytes -><- n-bytes     ->
> +------+------+------+----------------+
> | Info |    size     |  ... DATA .... |
> +------+------+------+----------------+

  -  Ack packets

> <-chan->
> +------+
> | Info |
> +------+

 The Info byte has a simple format.

> <- 5 bits -><-bit-><-2bit ->
> +----------+------+--------+ 
> | Channel  | A/B  | Type   |
> +----------+------+--------+

 Type

> 00 => Data 
> 01 => *Unused*
> 10 => Free
> 11 => Ack

-}
 
module Network.LambdaBridge.ARQ
        ( arqProtocol
        ) where

import Data.Word
import Data.Bits
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

import Debug.Trace

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout

-- TMP
import Network.LambdaBridge.Frame
import Data.Char

type PacketId  = Word16

{- | The 'FrameData' packet. It is always okay to fragment a packet, as long as the packets do not appear out of order. 

'FrameData' uses the following Binary format:

> <-byte-><- 2 bytes  -> <- 2 bytes ->
> +------+------+------+------+------+----------------+
> | 0x80 |  frame_id   |   size      |  ... DATA .... |
> +------+------+------+------+------+----------------+

-}

data FrameData = FrameData
		PacketId	-- the number of the packet
		BS.ByteString	-- the data (strict bytestring, not lazy)
	deriving (Show)

instance Binary FrameData where
	put (FrameData packId bs) = do
                putWord8 0x80
		put packId
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	get = do 0x80 <- getWord8 
                 packId <- get
                 sz <- get :: Get.Get Word16
	         bs <- Get.getByteString (fromIntegral sz)
                 return $ FrameData packId bs

{- |  The back-channel from data getting sent, on any specific channel,
   is always just 'Ack' and 'Free'. 'Free' implies 'Ack'.
 
'Ack' uses the following Binary format:

> <-byte-><- 2 bytes ->
> +------+------+------+
> | 0x00 |  frame_id   |                Ack
> +------+------+------+
>
> +------+------+------+
> | 0xff |  frame_id   |                Free
> +------+------+------+

-}

data Ack = Ack  PacketId	-- ^ packet I just got
	 | Free PacketId	-- ^ packet I got and forwarded
	deriving (Show)

instance Binary Ack where
        put (Ack packId) = do
                putWord8 0x00
                put packId
        put (Free packId) = do
                putWord8 0xff
                put packId

        get = do tag <- getWord8
                 packId <- get
                 case tag of
                   0x00 -> return $ Ack packId
                   0xff -> return $ Free packId

data Packet 
        = AckPacket Ack
        | DataPacket FrameData
	
instance Binary Packet where
        -- We rely on these two being distinct
	put (AckPacket ack) = put ack
	put (DataPacket dat) = put dat

	get = do tag <- lookAhead getWord8
		 case tag of
		   0x00 -> liftM AckPacket get
		   0x80 -> liftM DataPacket get
		   0xff -> liftM AckPacket get


-- | build a ByteString (packet'ed) API that uses these
-- the ARQ protocol.
arqProtocol :: Bridge Frame -> Limit -> IO (Bridge ByteString)
arqProtocol bridge tm = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)

	fastTimeout <- timeout tm
	slowTimeout <- timeout tm

        ackVar     <- newEmptyMVar :: IO (MVar Ack)
        dataVar    <- newEmptyMVar :: IO (MVar FrameData)

        forkIO $ forever $ do
                frame <- fromBridge bridge
                case fromFrame frame of
                  AckPacket ack  -> putMVar ackVar ack
                  DataPacket dat -> putMVar dataVar dat

	let getAck   = takeMVar ackVar
	let getData  = takeMVar dataVar

	let putData   = toBridge bridge . toFrame . DataPacket
	let putAck   = toBridge bridge . toFrame . AckPacket

	let startSend n = do
		bs <- takeMVar toSendVar
		readySend (FrameData n bs)

	    readySend dat@(FrameData n bs) = do
		putData dat
		res <- fastTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  Ack pid  | pid == n -> return $ ack
			  Free pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
		  Just (Ack _)  -> waitSend n
		  Just (Free _) -> startSend (n+1)
		  Nothing       -> readySend dat

	    waitSend n = do
		res <- slowTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  Free pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
		  Just (Free _) -> startSend (n+1)
				-- Assume that we just missed the free,and 
				-- revert to trying the next packet, with timeout.
		  Nothing       -> pingSend (n+1)
		
	    pingSend n = do
		putData (FrameData n BS.empty)
		res <- slowTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  Ack pid  | pid == n -> return $ ack
			  Free pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
			-- No room yet, so wait
		  Just (Ack _)  -> waitSend n
			-- Finally! got space
		  Just (Free _) -> startSend (n+1)
		  Nothing       -> pingSend n
		

	let start :: PacketId -> IO ()
	    start n = do
		dat <- getData
		recv'd n dat

	    recv'd :: PacketId -> FrameData -> IO ()
	    recv'd n (FrameData m bs) = 
                -- TODO: figure out what to do if n < m
                -- which is when the protocol has gone badly wrong (REBOOT NEEDED?)
                -- Also, make sure the loop round works
		if (m /= n) then do
			putAck (Free m) -- send a free, because this packet is already here
			start n
		      else do	-- m == n
			pushed <- tryPutMVar haveRecvVar $ bs
			if pushed then do
				putAck (Free n) -- send a free, because we've got and received the packet
				start (n+1)	-- and wait for the next packet
			      else do
				-- Mvar was full, so blocked
				putAck (Ack n) 	-- send a ack, because we *did* get the packet, even
						-- though we can't accept it yet.

				sync <- newEmptyMVar
				forkIO $ do
					putMVar haveRecvVar $ bs
					putAck (Free m)
					putMVar sync ()
				blocked n sync


	    blocked :: PacketId -> MVar () -> IO ()
	    blocked n sync = do
		FrameData m bs <- getData
		s <- tryTakeMVar sync
		case s of
		  -- Previous free sent, so can rejoin the start path, for a new packet
		  Just () -> recv'd (n+1) (FrameData m bs)
		  -- 'free' packet not yet sent
		  Nothing -> do
			if m == n then do
				putAck (Ack n)
			      else do
				return ()		-- send nothing; must be next packet
			blocked n sync


        -- This sends data
	forkIO $ startSend 0

	-- This sends acks
	forkIO $ start 0 

	return $ Bridge
	        { toBridge = putMVar toSendVar 
                , fromBridge = takeMVar haveRecvVar
	        }
	
