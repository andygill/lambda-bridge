
module Network.LambdaBridge.Multiplex where
	
import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import System.IO

import Network. LambdaBridge.Bridge

multiplex :: Word8 -> Bridge Frame Frame -> IO [Bridge Frame Frame]
multiplex n bridge = 
--	forkIO $ let pack <- 
	return $ 
	  [ Bridge 
	  	{ toBridge = \ (Frame bs) -> toBridge bridge (Frame (BS.cons i bs))
		, fromBridge = error ""
		}
	  | i <- take (fromIntegral n) [0..]
	  ]

