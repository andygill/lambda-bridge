{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, ScopedTypeVariables #-}
module Network.LambdaBridge.Bus where

import Network.LambdaBridge.Bridge


import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative
import Data.Monoid
import Data.Word
import Control.Monad.Writer
import Data.Map as Map

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

{-
data BusCmdPacket = BusCmdPacket U16 [BusCmd]

data BusCmd
        = WriteBus U16 U8               -- single read
        | ReadBus U16                   -- single write
        | ReplyBus U8                   -- Send back a specific msg

data BusReplyPacket = BusReplyPacket U16 [U8]
-}


data BusCmd :: * -> * where
        BusWrite :: Word16 -> Word8     -> BusCmd ()
        BusRead  :: Word16              -> BusCmd Word8

        Pure     :: a                   -> BusCmd a
        Apply    :: BusCmd (a -> b)
                 -> BusCmd a            -> BusCmd b

instance Applicative BusCmd where
        pure = Pure
        (<*>) = Apply

instance Monoid (BusCmd ()) where
        mempty = pure ()
        mappend b1 b2 = b1 *> b2

instance Functor BusCmd where
        fmap f a = pure f <*> a

newtype Board = Board (Frame -> IO (Maybe Frame))       -- Will time out in a Board-specific way
--                      ((Frame -> IO ()) -> IO ())       -- request callback on interupt

-- send, waits for response, can time out
send :: Board -> BusCmd a -> IO (Maybe a)
send (Board fn) cmds = do
        let req_msg = cmdToRequest cmds
        print req_msg
        rep_msg <- fn (Frame (BS.pack req_msg))
        print rep_msg
        case rep_msg of
          Nothing -> return Nothing
          Just (Frame rep) ->
                    case cmdWithReply cmds (BS.unpack rep) of
                        Just (a,[]) -> return (Just a)
                        Just (a,_)  -> return Nothing -- fail "bad format replied"
                        Nothing     -> return Nothing


cmdToRequest :: BusCmd a -> [Word8]
cmdToRequest (BusWrite addr val) = [tagWrite] ++ seq16 addr ++ [val]
cmdToRequest (BusRead addr)      = [tagRead] ++ seq16 addr
cmdToRequest (Pure a)            = []
cmdToRequest (Apply b1 b2)       = cmdToRequest b1 ++ cmdToRequest b2

cmdWithReply :: BusCmd a -> [Word8] -> Maybe (a,[Word8])
cmdWithReply (BusWrite {})      rep  = return ((),rep)
cmdWithReply (BusRead _)   (val:rep) = return (val,rep)
cmdWithReply (BusRead _)          [] = fail "bus reply error"
cmdWithReply (Pure a)           rep  = return (a,rep)
cmdWithReply (Apply b1 b2)      rep0 = do
        (f,rep1) <- cmdWithReply b1 rep0
        (a,rep2) <- cmdWithReply b2 rep1
        return (f a,rep2)

tagWrite, tagRead :: Word8
tagWrite = 0x1
tagRead = 0x2

hi, low :: Word16 -> Word8
hi = fromIntegral . (`div` 256)
low = fromIntegral . (`mod` 256)

seq16 :: Word16 -> [Word8]
seq16 v = [hi v,low v]

unseq16 :: [Word8] -> Word16
unseq16 [h,l] = fromIntegral h * 256 + fromIntegral l


test = do
        send brd $
                BusWrite 0 1    *>
                BusWrite 2 3    *>
                BusRead 5       <*
                BusWrite 9 10

brd = Board $ error ""
--boardFromBridgeFrame :: Bridge Frame -> IO Board

----------------------------------------------------------------

-- | connectBoard takes an initial timeout time,
--  and a Bridge Frame to the board, and returns
-- an abstact handle to the physical board.
connectBoard :: Float -> Bridge Frame -> IO Board
connectBoard timeoutTime bridge = do

        uniq :: MVar Word16 <- newEmptyMVar
        forkIO $ let loop n = do
                        putMVar uniq n
                        loop (succ n)
                 in loop 0

        callbacks :: Callback Word16 (Maybe Frame) <- liftM Callback $ newMVar Map.empty

        forkIO $ forever $ do
                Frame bs0 <- fromBridge bridge
                case do (a,bs1) <- BS.uncons bs0
                        (b,bs2) <- BS.uncons bs1
                        return (unseq16 [a,b],bs2) of
                  Just (uq,rest)
                           -- good packet, try respond
                        | BS.length rest > 0 -> callback callbacks uq (Just (Frame rest))
                  Nothing -> return () -- faulty packet?

        let send (Frame msg) = do
                uq <- takeMVar uniq

                rep :: MVar (Maybe Frame) <- newEmptyMVar

                -- register the callback
                register callbacks uq $ putMVar rep
                -- set up a delayed timeout response
                forkIO $ do
                        threadDelay (round (timeoutTime * 1000 * 1000))
                        callback callbacks uq Nothing

                toBridge bridge (Frame (BS.append (BS.pack (seq16 uq)) msg))

                -- And wait for the callback
                takeMVar rep


        return $ Board send

-- General code
data Callback k a = Callback (MVar (Map k (a -> IO ())))

register :: (Ord k) => Callback k a -> k -> (a -> IO ()) -> IO ()
register (Callback callbacks) key callback = do
        fm <- takeMVar callbacks
        putMVar callbacks (insert key callback fm)

callback :: (Ord k) => Callback k a -> k -> a -> IO ()
callback (Callback callbacks) key val = do
        fm <- takeMVar callbacks
        case Map.lookup key fm of
          Nothing -> putMVar callbacks fm       -- done
          Just f -> do
                putMVar callbacks (delete key fm)
                f val
