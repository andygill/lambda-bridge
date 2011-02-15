module Main where

import Data.Word
        
import Language.KansasLava

import Hardware.KansasLava.RS232

test1 = res
  where
        (back,res) = rs232out 9600 (1000 * 1000) (inp,())

        inp = toHandShaken [ Just (fromIntegral c) | c <-[0..]] back



test2 = {- take 257 $ -} [ x | Just x <- res3 ]
  where
	baud = 9600
	clkRate = 1000 * 1000 `div` 4
	clkRate' = 1000 * 1000 `div` 4

        inp = toHandShaken [ Just (fromIntegral c) | c <-[0..]] back
        (back,res) = rs232out 9600 clkRate (inp,())
        
        ((),res2)  = rs232in  9600 clkRate' (res,back2)
        (back2,res3) = fromHandShaken res2

main = print (zip test2 (map fromIntegral [0..] :: [Word8]))