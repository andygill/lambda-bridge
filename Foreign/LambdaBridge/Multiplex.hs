
module Foreign.LambdaBridge.Multiplex where
	
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

multiplex :: Bridge () Frame -> Bridge () Packet


