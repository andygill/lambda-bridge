{-# LANGUAGE RankNTypes #-}
-- | Implementation of ARQ; <http://en.wikipedia.org/wiki/Automatic_repeat_request> ''Stop-and-wait ARQ''.
-- Can be used to put a lightweight reliable link on top of an unreliable packet system, like UDP,
-- or a unreliable bytestream system like RS232.

module Network.LambdaBridge.ARQ
        ( sendWithARQ
        , recvWithARQ
        ) where

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

type PacketId  = Word16

{- | The 'FrameData' packet. It is always okay to fragment a packet, as long as the packets do not appear out of order. 

'FrameData' uses the following Binary format:

> <- 2 bytes  -> <- 2 bytes ->
> +------+------+------+------+----------------+
> |  frame_id   |   size      |  ... DATA .... |
> +------+------+------+------+----------------+

-}

data FrameData = FrameData
		PacketId	-- the number of the packet
		BS.ByteString	-- the data (strict bytestring, not lazy)
	deriving (Show)

instance Binary FrameData where
	put (FrameData packId bs) = do
		put packId
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	get = do packId <- get
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

sendWithARQ :: Bridge Frame -> Limit -> IO (ByteString -> IO ())
sendWithARQ bridge tm = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)

	fastTimeout <- timeout tm
	slowTimeout <- timeout tm
	let getAck    = liftM fromFrame $ fromBridge bridge
	let putData   = toBridge bridge . toFrame

	let start n = do
		bs <- takeMVar toSendVar
		ready (FrameData n bs)

	    ready dat@(FrameData n bs) = do
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
		  Just (Ack _)  -> wait n
		  Just (Free _) -> start (n+1)
		  Nothing       -> ready dat

	    wait n = do
		res <- slowTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  Free pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
		  Just (Free _) -> start (n+1)
				-- Assume that we just missed the free,and 
				-- revert to trying the next packet, with timeout.
		  Nothing       -> ping (n+1)
		
	    ping n = do
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
		  Just (Ack _)  -> wait n
			-- Finally! got space
		  Just (Free _) -> start (n+1)
		  Nothing       -> ping n
		
	forkIO $ start 0

	return $ putMVar toSendVar 

recvWithARQ :: Bridge Frame -> IO (IO ByteString)
recvWithARQ bridge = do
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)

	let getData  = liftM fromFrame $ fromBridge bridge
	let putAck   = toBridge bridge . toFrame

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

	-- This sends acks
	forkIO $ start 0 

	return $ takeMVar haveRecvVar

