
-- | Support for providing a Frame-based API on top of an unreliable bytestream.

module Network.LambdaBridge.Frame
	( frameProtocol
	, maxFrameSize
	, crc
	) where

import Control.Exception as E
import Data.Word
import Data.Bits
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import System.Timeout
import Numeric
import Data.Char
import Data.Default
import Data.Digest.CRC16
import Numeric

import Network.LambdaBridge.Logging (debugM)
import Network.LambdaBridge.Bridge

import System.Random
import Debug.Trace

{-

From http://en.wikipedia.org/wiki/Computation_of_CRC

If the data is destined for serial communication, it is best to use the bit ordering the data
will ultimately be sent in. This is because a CRC's ability to detect burst errors is based on
proximity in the message polynomial M(x); if adjacent polynomial terms are not transmitted
sequentially, a physical error burst of one length may be seen as a longer burst due to the
rearrangement of bits.

For example, both IEEE 802 (ethernet) and RS-232 (serial port) standards specify
least-significant bit first (little-endian) transmission, so a software CRC implementation to
protect data sent across such a link should map the least significant bits in each byte to
coefficients of the highest powers of x. On the other hand, floppy disks and most hard drives
write the most significant bit of each byte first.

We can test things with:

  http://www.lammertbies.nl/comm/info/crc-calculation.html

   CRC-CCITT (0xFFFF)

-}

-- | Compute the crc for a sequence of bytes.
-- The arguments for 'crc' are the crc code, and the initial value, often -1.
--
-- For example, CRC-16-CCITT, which is used for 'Frame', is crc 0x1021 0xffff.

-- Here we *explictly* use LSB first, which involved requesting that crc16Update
-- reverse the order of bits.

crc :: [Word8] -> Word16
crc = foldl (crc16Update 0x1021 True) 0xffff

-- | The maximum frame payload size, not including CRCs or headers, pre-stuffed.
maxFrameSize :: Int
maxFrameSize = 239

-----------------------------------------------------------------------
-- | 'frameProtocol' provides a Bridge Frame Frame from the services of a 'Bridge Byte'.
-- It is thread safe (two Frame writes to the same bridge will not garble each other)

{- |

It uses the following Frame format:


> <- sync+size -><- payload ...              ->|
> +------+------+----------------+------+------+
> | 0xfe |  sz  |  ... DATA .... |   CRC-16    |
> +------+------+----------------+------+------+


The '0xfe' is the sync marker starter; then comes the size (0 ...253)
then the CRC-16 for the [0xfe,sz]. This means that
when looking for a header, if a 0xfe is found in the sz position,
this marks a candidate for a new sync marker, and the one being
processed in corrupt.

Furthermore, the data is byte-stuffed
(<http://en.wikipedia.org/wiki/Bit_stuffing>)
for 0xfe, so 0xfe in DATA and the DATA-CRC is represented
using the pair of bytes 0xfe 0xff (which will never occur in a header).
In this way, if a byte is lost, the next header can be found
by scaning for 0xfe N, where N <= 253.

The arguments for 'frameProtocol' is the 'Bridge Byte' we uses to send the byte stream .

-}

frameProtocol :: Bridge Bytes -> IO (Bridge Frame)
frameProtocol bytes_bridge = do
        let debug = debugM "lambda-bridge.frame"

	let tag          = 0xf0 :: Word8
	    tag_stuffing = 0xff :: Word8

	-----------------------------------------------------------------------------
	sending <- newEmptyMVar

	let write wd = do
                debug $ "write 0x" ++ showHex wd ""
	        toBridge bytes_bridge (Bytes $ BS.pack [wd])


	let writeWithCRC xs = do
		let crc_val = crc xs
                debug $ "crc = " ++ showHex crc_val ""
		sequence_ [ do write x
			  | x <- xs ++ [ reverseBits $ fromIntegral $ crc_val `div` 256
				       , reverseBits $ fromIntegral $ crc_val `mod` 256
				       ]
			 ]

        let stuff x | x == tag  = [ tag, tag_stuffing ]
                    | otherwise = [ x ]

	let sender = do
		bs <- takeMVar sending
                debug $ "sending " ++ show bs
                write 0x0       -- to reset the RS232 stop bit alignment
                writeWithCRC
                        [ tag
                        , fromIntegral $ BS.length bs
                        ]
		writeWithCRC (concatMap stuff (BS.unpack bs))
		sender

	forkIO $ sender

	-----------------------------------------------------------------------------

	recving <- newEmptyMVar

        ch <- newChan

        forkIO $ let loop [] = do
                        Bytes wds <- fromBridge bytes_bridge
                        loop (BS.unpack wds)
                     loop (c:cs) = do
                        debug $ "read 0x" ++ showHex c ""
                        writeChan ch c
                 in loop []


	let read = readChan ch

	let checkCRC xs = crc xs == 0

	let findHeader0 :: IO ()
	    findHeader0 = do
		wd0 <- read	-- the first byte of a packet can wait as long as you like
                if wd0 == tag then findHeader1
                              else findHeader0

            -- found the tag byte; find the length
            findHeader1 :: IO ()
	    findHeader1 = do
		wd1 <- read
		findHeader2 wd1

	    -- you already have the two bytes,
	    -- and the first one was the tag
	    findHeader2 :: Word8 -> IO ()
	    findHeader2 wd1
		| fromIntegral wd1 <= maxFrameSize
		  = do debug $ "found header, len = " ++ show wd1
		       findHeaderCRC0 wd1
		| otherwise = do
                        debug $ "rejected header " ++ show [wd1]
			if wd1 == tag then findHeader1
                                      else findHeader0

	    readWithPadding :: IO Word8
	    readWithPadding = do
		wd1 <- read

		if wd1 == tag then
			do wd2 <- read
			   if wd2 == tag_stuffing then return wd1 else do
                                -- aborting this packet, start new packet
--                                print ("aborting packet (bad stuffing)")
                                debug $ "found non-stuffed tag inside packet"
				findHeader2 wd2
				fail "trampoline"
		    else do
			return wd1

            findHeaderCRC0 :: Word8 -> IO ()
            findHeaderCRC0 len = do
                   crc1 <- read
		   if crc1 == tag then findHeader1
                                  else findHeaderCRC1 len crc1

            findHeaderCRC1 :: Word8 -> Word8 -> IO ()
            findHeaderCRC1 len crc1 = do
                   crc2 <- read
                   case () of
                    _ | crc1 == tag -> findHeader1
                      | checkCRC [tag,len,crc1,crc2] -> do
                           debug $ "accepted header"
                           findPayload (fromIntegral len)
                      | otherwise -> do
                           debug $ "header crc failed (expecting " ++ show (crc [tag,len]) ++ ")"
                           findHeader0

	    findPayload :: Int -> IO ()
	    findPayload sz = do

		-- TODO: consider byte stuffing for 0xf1
		xs <- sequence  [ readWithPadding
		 		| i <- [1..(sz + 2)]
				]
		if checkCRC (concatMap stuff xs) then do
                        let frame = Frame (BS.pack (take sz xs))
                        debug $ "received packet " ++ show frame
                        putMVar recving frame
                        debug $ "forwarded packet"
		     else do
                        debug $ "crc failure 0x" ++ showHex (crc $ concatMap stuff xs) ""
			return ()
--		print xs
		return ()

	let readBridge = do
		findHeader0 `E.catch` \ SomeException {} -> do
--					print msg
					return ()
		readBridge


	forkIO $ readBridge

	return $ Bridge
		{ toBridge = \ (Frame bs) ->
			if BS.length bs > maxFrameSize
			then fail ("packet exceeded max frame size of " ++ show maxFrameSize)
			else putMVar sending bs
		, fromBridge = takeMVar recving
		}


-- Used to find the tag number (0xf0)
find = [ (tag,misses)
       | tag <- [1..255]
       , let misses = length
                [ ()
                | len <- [0..(tag - 1)]
                , let x  = crc [tag,len]
                , let hi = reverseBits $ fromIntegral $ x `div` 256
                , let low = reverseBits $ fromIntegral $ x `mod` 256
                , hi /= tag && low /= tag
                ]
       , misses == fromIntegral tag
       ]


reverseBits :: Word8 -> Word8
reverseBits x = sum [ 2^(7-i)
                    | i <- [0..7]
                    , x `testBit` i
                    ]


-- x^16 + x^12 + x^5 + 1
crcMe :: [Word8] -> Word16
crcMe cs = loop 0xffff cs
  where
        loop r (c:cs) = loop (loop2 (r `xor` fromIntegral c) c 0) cs
        loop r [] = r

        loop2 r c 8 = r
        loop2 r c i = if r `testBit` i
                    then loop2 ((r `shiftR` 1) `xor` 0x8408) c (i+1)
                    else loop2 (r `shiftR` 1)                c (i+1)

-- 0x1021
-- 0x8408


{-

 function crc(byte array string[1..len], int len) {
     rem := 0
     // A popular variant complements rem here
     for i from 1 to len {
         rem := rem xor string[i]
         for j from 1 to 8 {   // Assuming 8 bits per byte
             if rem and 0x0001 {   // if rightmost (least significant) bit is set
                 rem := (rem rightShift 1) xor 0x8408
             } else {
                 rem := rem rightShift 1
             }
         }
     }
     // A popular variant complements rem here
     return rem
-}


data BOOL = B String Bool

instance Show BOOL where
--        show (B xs True) = xs ++ "<1>"
--        show (B xs False) = xs ++ "<0>"
        show (B xs True) = "1"
        show (B xs False) = "0"

xOR :: BOOL -> BOOL -> BOOL
xOR (B x x') (B y y') = B (par x ++ "^" ++ par y) (x' /= y')
  where par [x] = [x]
        par other = "(" ++ other ++ ")"

false = B "0" False
true  = B "1" True

str = map (\ x -> if x == '0' then false else true)

-- 0x1021
-- 0x8408

--crc_spec :: [Bool] -> Bool -> [Bool]
-- You need to step this 8 times; after xoring the low bits with the input byte.
crc_spec :: [BOOL] -> [BOOL]
crc_spec bs   = [ if x `elem` [12,5,0]
                  then xOR b (last bs) -- b /= last bs
                  else b
                | (x,b) <- [0..] `zip` (false : init bs)
                ]

-- test txt n = reverse $ foldl crc_spec (reverse (str txt)) (replicate n false)
--test n t = reverse $ foldl crc_spec (replicate 16 true) (replicate n false ++ [t])