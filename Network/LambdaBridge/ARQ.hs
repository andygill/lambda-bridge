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

--------------------------------------------------------------------------

data Hdr = Hdr
        { hdr_tag     :: TagId
        , hdr_type    :: TypeId
        }
        deriving (Eq, Ord, Show)
        
data TagId     = A | B
        deriving (Eq, Ord, Show)

otherTag :: TagId -> TagId
otherTag A = B
otherTag B = A

data TypeId 
        = DataId
        | ReservedId
        | FreeId 
        | AckId 
        deriving (Eq, Ord, Show)
        
instance Binary Hdr where
	put (Hdr tag ty) = 
                putWord8 $ (case tag of { A -> 0x0 ; B -> 0x4 })
                      .|.  (case ty of { DataId -> 0x0; ReservedId -> 0x1 ; FreeId -> 0x2 ; AckId -> 0x3 })
        get = do
                wd <- getWord8
                return $ Hdr (if testBit wd 2 then B else A)
                             (case wd .&. 0x3 of
                                  0x0 -> DataId
                                  0x2 -> FreeId
                                  0x3 -> AckId
                                  _   -> ReservedId)
                             

frameDecode :: Frame -> Either DataPacket AckPacket
frameDecode (Frame bs) = run $ do
--        () <- trace (show ("decode",bs)) $ return ()
        hdr <- lookAhead get
        r <- case hdr_type hdr of
           DataId -> liftM Left get 
           AckId  -> liftM Right get
           FreeId -> liftM Right get
--        () <- trace (show ("decoded as ",r)) $ return ()
        return r
                  
  where lbs = LBS.fromChunks [bs]
        run m = runGet m lbs
                

frameDataPacket :: DataPacket -> Frame
frameDataPacket = toFrame

frameAckPacket :: AckPacket -> Frame
frameAckPacket = toFrame


--------------------------------------------------------------------------

data DataPacket = DataPacket 
                TagId           -- ^ The tag id
		BS.ByteString	-- the data (strict bytestring, not lazy)
        deriving (Eq,Ord,Show)

instance Binary DataPacket where
	put (DataPacket tag bs) = do
		put (Hdr tag DataId)
		put (fromIntegral (BS.length bs) :: Word16)
		putByteString bs
	get = do hdr <- get
                 case hdr_type hdr of
                   DataId -> do
                           sz <- get :: Get.Get Word16
	                   bs <- Get.getByteString (fromIntegral sz)
                           return $ DataPacket (hdr_tag hdr) bs
                   _ -> fail "expecting data, found non-data tag"

--------------------------------------------------------------------------

data AckPacket = AckPacket TagId 
               | FreePacket TagId
        deriving (Eq,Ord,Show)

instance Binary AckPacket where
	put (AckPacket tag)  = put (Hdr tag AckId)
	put (FreePacket tag) = put (Hdr tag FreeId)
	get = do hdr <- get
                 case hdr_type hdr of
                   AckId  -> return $ AckPacket (hdr_tag hdr)
                   FreeId -> return $ FreePacket (hdr_tag hdr)
                   _ -> fail "expecting ack or free, found other"



--------------------------------------------------------------------------
{-
type PacketId  = Word16

{- | The 'FrameData' packet. 
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

data Packet'
        = AckPacket' Ack
        | DataPacket' FrameData
	
instance Binary Packet' where
        -- We rely on these two being distinct
	put (AckPacket' ack) = put ack
	put (DataPacket' dat) = put dat

	get = do tag <- lookAhead getWord8
		 case tag of
		   0x00 -> liftM AckPacket' get
		   0x80 -> liftM DataPacket' get
		   0xff -> liftM AckPacket' get
-}

-- | build a ByteString (packet'ed) API that uses these
-- the ARQ protocol.
arqProtocol :: Bridge Frame -> Limit -> IO (Bridge ByteString)
arqProtocol bridge tm = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)

	fastTimeout <- timeout tm
	slowTimeout <- timeout tm

        ackVar     <- newEmptyMVar :: IO (MVar AckPacket)
        dataVar    <- newEmptyMVar :: IO (MVar DataPacket)

        forkIO $ forever $ do
                frame <- fromBridge bridge
                case frameDecode frame of
                  Right ack  -> putMVar ackVar ack
                  Left  dat  -> putMVar dataVar dat


	let getAck   = takeMVar ackVar
	let getData  = takeMVar dataVar

	let putData   = toBridge bridge . frameDataPacket
	let putAck   = toBridge bridge . frameAckPacket

	let startSend n = do
		bs <- takeMVar toSendVar
		readySend (DataPacket n bs)

	    readySend dat@(DataPacket n bs) = do
		putData dat
		res <- fastTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  AckPacket pid  | pid == n -> return $ ack
			  FreePacket pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
		  Just (AckPacket _)  -> waitSend n
		  Just (FreePacket _) -> startSend (otherTag n)
		  Nothing             -> readySend dat

	    waitSend n = do
		res <- slowTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  FreePacket pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
		  Just (FreePacket _) -> startSend (otherTag n)
				-- Assume that we just missed the free,and 
				-- revert to trying the next packet, with timeout.
		  Nothing       -> pingSend (otherTag n)
		
	    pingSend n = do
		putData (DataPacket n BS.empty)
		res <- slowTimeout $ 
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  AckPacket pid  | pid == n -> return $ ack
			  FreePacket pid | pid == n -> return $ ack
			  _ -> loop
		  in loop
		case res of
			-- No room yet, so wait
		  Just (AckPacket _)  -> waitSend n
			-- Finally! got space
		  Just (FreePacket _) -> startSend (otherTag n)
		  Nothing       -> pingSend n
		

	let start :: TagId -> IO ()
	    start n = do
		dat <- getData
		recv'd n dat

	    recv'd :: TagId -> DataPacket -> IO ()
	    recv'd n (DataPacket m bs) = 
                -- TODO: figure out what to do if n < m
                -- which is when the protocol has gone badly wrong (REBOOT NEEDED?)
                -- Also, make sure the loop round works
		if (m /= n) then do
			putAck (FreePacket m) -- send a free, because this packet is already here
			start n
		      else do	-- m == n
			pushed <- tryPutMVar haveRecvVar $ bs
			if pushed then do
				putAck (FreePacket n) -- send a free, because we've got and received the packet
				start (otherTag n)	-- and wait for the next packet
			      else do
				-- Mvar was full, so blocked
				putAck (AckPacket n) 	-- send a ack, because we *did* get the packet, even
						-- though we can't accept it yet.

				sync <- newEmptyMVar
				forkIO $ do
					putMVar haveRecvVar $ bs
					putAck (FreePacket m)
					putMVar sync ()
				blocked n sync


	    blocked :: TagId -> MVar () -> IO ()
	    blocked n sync = do
		DataPacket m bs <- getData
		s <- tryTakeMVar sync
		case s of
		  -- Previous free sent, so can rejoin the start path, for a new packet
		  Just () -> recv'd (otherTag n) (DataPacket m bs)
		  -- 'free' packet not yet sent
		  Nothing -> do
			if m == n then do
				putAck (AckPacket n)
			      else do
				return ()		-- send nothing; must be next packet
			blocked n sync


        -- This sends data
	forkIO $ startSend A

	-- This sends acks
	forkIO $ start A

	return $ Bridge
	        { toBridge = putMVar toSendVar 
                , fromBridge = takeMVar haveRecvVar
	        }
