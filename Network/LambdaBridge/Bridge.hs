{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Network.LambdaBridge.Bridge where

import Data.Word as W
import System.Random
import Data.Default
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Numeric
import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time.Clock
import Control.Exception as Exc
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import System.IO.Unsafe (unsafeInterleaveIO)

-- | A 'Bridge' is a bidirectional connection to a specific remote API.
-- There are many different types of Bridges in the lambda-bridge API.

data Bridge msg = Bridge 
	{ toBridge 	:: msg -> IO ()	-- ^ write to a bridge; may block; called many times.
	, fromBridge	:: IO msg	-- ^ read from a bridge; may block, called many times.
					--   The expectation is that *eventually* after some
					--   time and/or compuation
					--   someone will read from the bridge, and the
					--   reading does not depend on any external interaction or events.
	}

-- | A 'BridgePort' is the port number of a remote FIFO/pipe,
-- where  0 is (reserved) controller, 1 is the main fifo.

type BridgePort = Word8

-- | A 'Bridge (of) Byte' is for talking one byte at a time, where the
-- byte may or may not get there, and may get garbled.
--
-- An example of a 'Bridge (of) Byte Byte' is a RS-232 link.

newtype Byte = Byte W.Word8 deriving (Eq,Ord)

instance Show Byte where
   show (Byte w) = "0x" ++ showHex w ""

-- | A 'Bridge (of) Frame' is small set of bytes, where a Frame may
-- or may not get to the destiation, but if receieved, will 
-- not be garbled or fragmented (via CRC or equiv).
-- There is typically an implementation specific maximum size of a Frame.

-- An example of a 'Bridge (of) Frame Frame' is UDP.

newtype Frame = Frame BS.ByteString 
instance Show Frame where
   show (Frame wds) = "Frame " ++ show [ Byte w | w <- BS.unpack wds ]

-- | A way of turning a Frame into its contents, using the 'Binary' class.
-- This may throw an async exception.
fromFrame :: (Binary a) => Frame -> a
fromFrame (Frame fs) = decode (LBS.fromChunks [fs])

-- | A way of turning something into a Frame, using the 'Binary' class.
toFrame :: (Binary a) => a -> Frame
toFrame a = Frame $ BS.concat $ LBS.toChunks $ encode a

-- | A 'Link' is a sequence of ByteString that can be 
-- sub-divided if needed. Its just a sequence in a bundle for 
-- transportation. 

-- TODO: rename as Channel

newtype Link = Link BS.ByteString 
instance Show Link where
   show (Link wds) = "Link " ++ show [ Byte w | w <- BS.unpack wds ]

-- | ''realisticBridge'' is a way of making a 'Bridge' less sterile, for testing purposes.
--- currently only effects the outgoing direction.
realisticBridge :: (Show msg) => Realistic msg -> Realistic msg -> Bridge msg -> IO (Bridge msg)
realisticBridge send recv sock = do
	let you :: Float -> IO Bool
	    you f = do
		r <- randomIO
		return $ f > r

	let optMangle f mangle a = do
		b <- you f
		if b then do
			g <- newStdGen
			return $ mangle g a
		     else return a 
	tm <- getCurrentTime
	tmVar <- newMVar tm

        let unrely :: Realistic msg -> msg -> (msg -> IO ()) -> IO ()
            unrely opts a k = do
		tm0 <- takeMVar tmVar	-- old char time
		tm1 <- getCurrentTime	-- current time
		let pause = pauseU opts - realToFrac (tm1 `diffUTCTime` tm0)
		if pause <= 0 then return () else do
		   threadDelay (floor (pause * 1000 * 1000))
		   return ()
		b <- you (loseU opts)
		if b then return () else do -- ignore if you "lose" the message.
		  a <- optMangle (mangleU opts) (mangler opts) a
		  b <- you (dupU send)
		  if b then do		   -- send twice, please
		  	k a			
		  	k a			
		       else do
	                k a
		tm <- getCurrentTime
		putMVar tmVar tm
		return ()
        
        backChan <- newChan

        forkIO $ forever $ do
                msg <- fromBridge sock
                unrely recv msg $ writeChan backChan

  	return $ Bridge
	  { toBridge = \ a -> unrely send a $ toBridge sock
	  , fromBridge = readChan backChan -- fromBridge sock
	  }

-- |  ''Realistic'' is the configuration for ''realisticBridge''.
data Realistic a = Realistic
	{ loseU 	:: Float	-- ^ lose an 'a'
	, dupU 		:: Float	-- ^ dup an 'a'
	, execptionU 	:: Float	-- ^ throw exception instead
	, pauseU	:: Float	-- ^ what is the pause between things
	, mangleU 	:: Float	-- ^ mangle an 'a'
	, mangler :: forall g . (RandomGen g) => g -> a -> a
	}

-- | default instance of 'realistic', which is completely reliable.
instance Default (Realistic a) where
	def = Realistic 0 0 0 0 0 (\ g a -> a)

-- | 'debugBridge' outputs to the stderr debugging messages
-- about what datum is getting send where.

debugBridge :: (Show msg) => String -> Bridge msg -> IO (Bridge msg)
debugBridge name bridge = do
	sendCounter <- newMVar 0
	recvCounter <- newMVar 0

	return $ Bridge
		{ toBridge = \ a -> do
			count <- takeMVar sendCounter
			putMVar sendCounter (succ count)
			putStrLn $ name ++ ":toBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			() <- toBridge bridge a
			putStrLn $  name ++ ":toBridge<" ++ show count ++ "> success"
		, fromBridge = do
			count <- takeMVar recvCounter
			putMVar recvCounter (succ count)
			putStrLn $  name ++ ":fromBridge<" ++ show count ++ ">"
			a <- fromBridge bridge `Exc.catch` \ (e :: SomeException) -> do { print e ; throw e }
			putStrLn $  name ++ ":fromBridge<" ++ show count ++ "> (" ++ show a ++ ")"
			return a
		}

-- | 'loopbackBridge' is a simple reflective loopback. It obeys the basic
-- permise of a bridge; if no-one is listening, then data can get lost.
-- It is in the nature of the bridge; things do not always make it.

loopbackBridge :: IO (Bridge a)
loopbackBridge = do
	var <- newEmptyMVar
	return $ Bridge
		{ toBridge = \ a -> do
			putMVar var a
		, fromBridge = do
			takeMVar var
		}

-- | 'pipeBridge' create two ends of a polymorphic 'Bridge'.
-- This link is decoupled, in the sense that if no-one is listening,
-- then the element in transport gets dropped.

pipeBridge :: (Show a) => Int -> Float -> IO (Bridge a, Bridge a)
pipeBridge max_len delay = do
        sending <- newEmptyMVar
        recving <- newEmptyMVar

        sending' <- newChan
        recving' <- newChan

        tm <- getCurrentTime

        -- We want the sending process to *wait* until we are ready to send
        let transport tm0 extra from to = do
                v <- takeMVar from
                writeChan to v
                -- Now I've send the packet, how long do I wait before sending
                -- the next one?
                tm1 <- getCurrentTime
                let diff :: Float
                    diff = fromRational (toRational (tm1 `diffUTCTime` tm0))

                let extra1 = max (extra + delay - diff) 0

--                print (extra1,extra,delay,diff)

                when (extra1 > 0.02) $ do
                        threadDelay (floor (extra1 * 1000 * 1000))
                                                                   
                transport tm1 (extra1 * 0.99) from to

        forkIO $ transport tm 0 sending sending'
        forkIO $ transport tm 0 recving recving'

	let b1 = Bridge
		{ toBridge   = putMVar sending
		, fromBridge = readChan recving'
		}
	let b2 = Bridge
		{ toBridge   = putMVar recving
		, fromBridge = readChan sending'
		}

	return (b1,b2)


stubBridge :: [a] -> IO (Bridge a)
stubBridge recv = do
	var <- newMVar recv
	return $ Bridge
		{ toBridge = \ a -> return ()
		, fromBridge = do
			(r:rs) <- takeMVar var
			putMVar var rs
			return r
		}


basicByteBridge :: ([Maybe Word8] -> [Maybe Word8]) -> IO (Bridge Byte)
basicByteBridge fn = do
        ins <- newEmptyMVar :: IO (MVar Byte)
        out <- newEmptyMVar :: IO (MVar Byte)

        let prod :: IO [Maybe Word8]
            prod = unsafeInterleaveIO $ do
	                x <- tryTakeMVar ins
	                xs <- prod
	                return (fmap (\ (Byte w) -> w) x:xs)

        inp <- prod

        let res = fn inp

        forkIO $ sequence_ [ putMVar out (Byte b) | Just b <- res ]

        return $ Bridge 
                { toBridge = putMVar ins
                , fromBridge = takeMVar out
                }

-- | 'byteBridgeToLinkBridge' builds a trivial network (Link) stack.
byteBridgeToLinkBridge :: (Bridge Byte) -> IO (Bridge Link)
byteBridgeToLinkBridge bridge = do
         ins <- newChan :: IO (Chan Link)
         out <- newChan :: IO (Chan Link)

         forkIO $ forever $ do
                (Link bs) <- readChan ins
                sequence_ [ toBridge bridge (Byte b) | b <- BS.unpack bs ]

         forkIO $ forever $ do
                (Byte b) <- fromBridge bridge
                writeChan out (Link $ BS.pack [b])

         return $ Bridge 
                { toBridge = writeChan ins
                , fromBridge = readChan out
                }

