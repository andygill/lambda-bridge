
-- | Support for providing a Frame-based API on top of an unreliable bytestream.

module Network.LambdaBridge.Frame
--	( crc
--	, maxFrameSize
--	, frameProtocol
--	) where
	where

import Data.Word
import Data.Bits
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import System.Timeout 
import Numeric
import Data.Char
import Data.Default


import Network.LambdaBridge.Bridge

import System.Random
import Debug.Trace

{-

Frame format:

<pre>
  <- sync + size            ->|<- payload ...              ->|
  +------+------+------+------+----------------+------+------+
  | 0xf1 |  sz  |   CRC-16    |  ... DATA .... |   CRC-16    |
  +------+------+------+------+----------------+------+------+
</pre>

The '0xe4' is the sync marker starter; then comes the size (1 ...217),
then the CRC-16 for the [0xf1,sz]. This CRC-16 will never contain 0xf1
in either byte (causing the upto 238 restriction). This means that
when looking for a header, if a 0xe4 is found in the sz or CRC positions,
this marks a candidate for a new sync marker, and the one being
processed in corrupt.

-}

-- | Compute the crc for a sequence of bytes, assuming LSB processing.
-- The arguments are the crc code, and the initial value, often -1.
--
--For example, CRC-16-CCITT, LSB first (for use with RS-232), is crc 0x1021 0xffff.

crc :: (Bits w) => w -> w -> [Word8] -> w
crc code start bs = foldl (\ crc b -> 
			     let b' = if b then 1 else 0
				 crc' = (crc `shiftL` 1) `xor` b'
			     in if crc `testBit` (bitSize crc - 1)
				then crc' `xor` code
			        else crc'
		 ) start
		 $ concat [  [ b `testBit` i
			    | i <- reverse [0..7]
			    ] 
			  | b <- bs
			  ]

-- | The maximum frame payload size, not including CRC.
maxFrameSize :: Int
maxFrameSize = 238

-- This attempts all possible headers, and checks to see if there are any 
-- 0xf1's (the tag) appearing in the size or checksum. Not exported.

sanityCheck = 
	[
	       let v = crc (0x1021 :: Word16) 0xffff [tag,sz,0,0]
	           b = fromIntegral (v `div` 256) :: Word8
	           a = fromIntegral v :: Word8
 	           v' = crc (0x1021 :: Word16) 0xffff [tag,sz,b,a]
               in if a == tag || b == tag || sz == tag || v' /= 0
	       then show (showH b,showH a,showH v',sz)
	       else "-" -- good outcome

	 | sz <- [0..fromIntegral maxFrameSize]
	 ]
  where
	tag :: Word8
	tag = 0xf1

-----------------------------------------------------------------------
-- | 'frameProtocol' provides a Bridge Frame Frame from the services of a 'Bridge Byte Byte'.
-- It is thread safe (two Frame writes to the same bridge will not garble each other)

frameProtocol :: Double -> Bridge Byte Byte -> IO (Bridge Frame Frame)
frameProtocol tmOut byte_bridge = do

	-----------------------------------------------------------------------------
	sending <- newEmptyMVar

	let write wd = toBridge byte_bridge (Byte wd)

	let writeWithCRC xs = do
		sequence_ [ write x | x <- xs ]
		let crc_val = crc (0x1021 :: Word16) 0xffff (xs ++ [0,0])
		write (fromIntegral (crc_val `div` 256))
		write (fromIntegral (crc_val `mod` 256))

	let sender = do
		bs <- takeMVar sending
		let hdr :: [Word8]
		    hdr = [0xf1,fromIntegral $ BS.length bs]
		writeWithCRC hdr
		writeWithCRC (BS.unpack bs)
		sender
		
	forkIO $ sender

	-----------------------------------------------------------------------------
	let tag = 0xf1 :: Word8

	recving <- newEmptyMVar

	-- TODO: these three defs can be folded
	let read = do Byte wd <- fromBridge byte_bridge
		      return wd

	    -- timeout in microseconds
	let tmOut' :: Int
	    tmOut' = floor (tmOut * 1000 * 1000)

	let step = do
		wd' <- timeout tmOut' read 
		case wd' of
		  Nothing -> fail ("step timed out")
		  Just wd -> return wd
	
	let checkCRC xs = crc (0x1021 :: Word16) 0xffff xs == 0 

	let findHeader :: IO ()
	    findHeader = do
		wd0 <- step
		wd1 <- step
		wd2 <- step
		wd3 <- step
		findHeader' wd0 wd1 wd2 wd3
		
	    -- you already have the first byte
	    findHeader' :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()
	    findHeader' wd0 wd1 wd2 wd3 
		| wd0 == 0xf1 && wd1 == 0 && trace "0xf1" False = undefined
		| wd0 == 0xf1
		  && fromIntegral wd1 <= maxFrameSize 
		  && checkCRC [wd0,wd1,wd2,wd3] = findPayload (fromIntegral wd1)
		| otherwise = do
			wd4 <- step
	    		findHeader' wd1 wd2 wd3 wd4

	    findPayload :: Int -> IO ()
	    findPayload sz = do
		print sz

		-- TODO: consider byte stuffing for 0xf1
		xs <- sequence [ step
			       | i <- [1..(sz + 2)]
			       ]
--		print xs
		if checkCRC xs then putMVar recving (Frame (BS.pack (take sz xs)))
			       else return ()
--		print xs
		return ()

	let readBridge = do
		findHeader `catch` \ msg -> do
--					print msg
					return ()
		readBridge


	forkIO $ readBridge

	return $ Bridge 
		{ toBridge = \ (Frame bs) ->
			if BS.length bs > maxFrameSize
			then fail "packet exceeded max frame size"
			else putMVar sending bs
		, fromBridge = takeMVar recving
		}



-----------------------------------------------------------------------


-- example1: CRC-CCITT (0x1D0F)	0xE5CC
-- starts with 0xffff, 0x1D0F is output if no input
-- or appending 

example1 = [49..57]
example1_crc = crc (0x1021 :: Word16) 0xffff (example1 ++ [0,0])

example2 = example1 ++ [0xe5,0xcc]
example2_crc = crc (0x1021 :: Word16) 0xffff example2

showH x = "0x" ++ showHex x ""


main :: IO ()
main = do
	bridge_byte <- loopbackBridge
	
--	bridge_byte' <- debugBridge bridge_byte
	let u = def { loseU = 0.001, dupU = 0.000, mangleU = 10.001, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte' <- unreliableBridge u def bridge_byte

--	sequence_ [ toBridge bridge_byte' x | x <- [0..255]]

	bridge_frame <- frameProtocol 0.0001 bridge_byte'

	forkIO $ do
		sequence [ toBridge bridge_frame $ Frame (toStr $ "Frame: " ++ show i ++ " " ++ ['a'..'z'] ++ [chr 0xf1])
			 | i <- [1..]
			 ]
		return ()
		
	sequence [ do
		frame <- fromBridge bridge_frame
		print frame
			| _ <- [0..1000]]

	return ()

toStr :: String -> BS.ByteString
toStr = BS.pack . map (fromIntegral . ord)
