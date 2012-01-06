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
        { hdr_tag     :: PacketId
        , hdr_type    :: TypeId
        }
        deriving (Eq, Ord, Show)

data PacketId     = A | B
        deriving (Eq, Ord, Show)

otherTag :: PacketId -> PacketId
otherTag A = B
otherTag B = A

data TypeId
        = DataId
        | Ack AckId
        deriving (Eq, Ord, Show)

data AckId
        = AckId
        | PauseId
        | UnpauseId
        deriving (Eq, Ord, Show)

instance Binary Hdr where
	put (Hdr tag ty) =
                putWord8 $ (case tag of { A -> 0x0 ; B -> 0x4 })
                      .|.  (case ty of { DataId -> 0x0; Ack AckId -> 0x1 ; Ack PauseId -> 0x2 ; Ack UnpauseId -> 0x3 })
        get = do
                wd <- getWord8
                return $ Hdr (if testBit wd 2 then B else A)
                             (case wd .&. 0x3 of
                                  0x0 -> DataId
                                  0x1 -> Ack AckId
                                  0x2 -> Ack PauseId
                                  0x3 -> Ack UnpauseId
                                  _   -> error "impossible (getting Hdr from binary)")


frameDecode :: Frame -> Either DataPacket AckPacket
frameDecode (Frame bs) = run $ do
--        () <- trace (show ("decode",bs)) $ return ()
        hdr <- lookAhead get
        r <- case hdr_type hdr of
           DataId -> liftM Left get
           _      -> liftM Right get
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
                PacketId          -- The tag id
		BS.ByteString	  --  the data (strict bytestring, not lazy)
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

data AckPacket = AckPacket PacketId AckId
        deriving (Eq,Ord,Show)

instance Binary AckPacket where
	put (AckPacket ackid tag)  = put (Hdr ackid (Ack tag))
	get = do hdr <- get
                 case hdr_type hdr of
                   Ack ackid  -> return $ AckPacket (hdr_tag hdr) ackid
                   DataId    -> fail "expecting ack style packet, found a data packet"

--------------------------------------------------------------------------

-- | build a ByteString (packet'ed) API that uses these
-- the ARQ protocol.
arqProtocol :: Bridge Frame -> Limit -> IO (Bridge ByteString)
arqProtocol bridge tm = do
	toSendVar   <- newEmptyMVar :: IO (MVar ByteString)
	haveRecvVar <- newEmptyMVar :: IO (MVar ByteString)

	sendTimeout <- timeout tm
	unpauseTimeout <- timeout tm

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

        --------------------------------------------------
	let startSend n = do
		bs <- takeMVar toSendVar
		readySend (DataPacket n bs)

	    readySend dat@(DataPacket n bs) = do
		putData dat
		sentSend dat

            sentSend dat@(DataPacket n _) = do
		res <- sendTimeout $
		  let loop = do
			ack <- getAck
			case ack of
			  -- only accept ack's with the correct pid's
			  AckPacket pid ackType | pid == n -> return $ ackType
			  _ -> loop
		  in loop
		case res of
		  Just AckId      -> startSend (otherTag n)
		  Just PauseId    -> waitSend n
		  Just UnpauseId  -> ackSend n
		  Nothing         -> readySend dat

            waitSend n = do
--                print ("waitSend",n)
		ack <- getAck
                case ack of
                  AckPacket pid UnpauseId
                        | pid == n -> ackSend n
                  _ -> error $ "waitSend, found " ++ show ack ++ ", packet id = " ++ show n

            ackSend n = do
                    obs <- tryTakeMVar toSendVar
                    readySend $ DataPacket (otherTag n)
                              $ case obs of
                                 Nothing -> BS.empty
                                 Just bs -> bs

        --------------------------------------------------
{-

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

-}

        let startRecv :: PacketId -> IO ()
            startRecv m = do
		dat@(DataPacket pid _) <- getData
                if m == pid then recv'dRecv dat
                            else ackRecv pid

            ackRecv m = do
                putAck (AckPacket m $ AckId)
                startRecv (otherTag m)

            recv'dRecv dat@(DataPacket pid bs) = do
                pushed <- tryPutMVar haveRecvVar bs
                if pushed then do
                        ackRecv pid
                    else do
--                        print ("Sending pause")
                        putAck (AckPacket pid $ PauseId)
                        pausingRecv dat

            pausingRecv (DataPacket pid bs) = do
                putMVar haveRecvVar bs
                unpausingRecv pid

            unpausingRecv m = do
                putAck (AckPacket m $ UnpauseId)
                unpausedRecv m

            unpausedRecv m = do
                optDat <- unpauseTimeout $ getData
                case optDat of
                  Nothing -> unpausingRecv m
                  Just dat@(DataPacket pid _)
                     | m == pid  -> ackRecv pid
                     | otherwise -> recv'dRecv dat

            {-

	let start :: PacketId -> IO ()
	    start n = do
		dat <- getData
		recv'd n dat

	    recv'd :: PacketId -> DataPacket -> IO ()
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


	    blocked :: PacketId -> MVar () -> IO ()
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


-}
        -- This sends data
	forkIO $ startSend A

	-- This sends acks
	forkIO $ startRecv A

	return $ Bridge
	        { toBridge = putMVar toSendVar
                , fromBridge = takeMVar haveRecvVar
	        }
