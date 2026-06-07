-- | Internal high-level serial port
module Sport.Sport
  ( Sport
  , withNewSport
  , newSportIO
  , newSport
  , withSport
  , openSport
  , isOpenSport
  , getSportCfg
  , defSportCfg
  , SportCfg(..)
  , closeSport
  , readSport
  , readSomeSport
  , writeSport
  , flushSport
  , SportException(..)
  , displaySportException
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Sport.Serial as S
import System.IO
import System.Posix

-- | Serial port handle
data Sport = Sport
  { cfgVar  :: TMVar SportCfg
  , hndlVar :: TMVar Handle
  }

-- | Acquire IO serial port
newSportIO :: IO Sport
newSportIO = atomically newSport

-- | Acquire STM serial port
newSport :: STM Sport
newSport = Sport <$> newEmptyTMVar <*> newEmptyTMVar

-- | Check if serial port is open
isOpenSport :: Sport -> STM Bool
isOpenSport = fmap not . isEmptyTMVar . cfgVar

-- | Handle and serial configuration
data SportCfg = SportCfg
  { binaryMode :: Bool -- ^ Binary mode True, text mode False.
  , bufferMode :: BufferMode
  , path       :: FilePath
  , speed      :: BaudRate
  , byteSize   :: Int -- ^ number of bits per byte
  , parity     :: Maybe S.Parity
  , stopBits   :: S.StopBits
  , rtimeout   :: Maybe Int
  , excl       :: Bool -- ^ exclusive
  }
  deriving (Eq, Show)

-- | Binary with no buffering
defSportCfg :: SportCfg
defSportCfg =
  SportCfg
    True
    NoBuffering
    (S.path S.defSerialCfg)
    (S.speed S.defSerialCfg)
    (S.byteSize S.defSerialCfg)
    (S.parity S.defSerialCfg)
    (S.stopBits S.defSerialCfg)
    (S.rtimeout S.defSerialCfg)
    (S.excl S.defSerialCfg)

-- | Acquire new handle 'newSportIO' then bracket
-- 'openSport' with a configuration to 'closeSport'.
withNewSport :: SportCfg -> (Sport -> IO a) -> IO a
withNewSport cfg k = do
  s <- newSportIO
  withSport s cfg $ k s

-- | Bracket 'openSport' and 'closeSport'.
withSport :: Sport -> SportCfg -> IO a -> IO a
withSport s cfg = bracket_ (openSport s cfg) (closeSport s)

-- | Open the serial port. Throw 'SportAlreadyOpen' if the
-- serial port was already opened.
openSport :: Sport -> SportCfg -> IO ()
openSport s cfg' =
  bracketOnError (S.openSerial $ toSerialCfg cfg') hClose $ \serial -> do
    hSetBinaryMode serial $ binaryMode cfg'
    hSetBuffering  serial $ bufferMode cfg'
    atomically $ do
      cfgM <- getSportCfg s
      case cfgM of
        Just cfg -> throwSTM $ SportAlreadyOpen $ path cfg
        Nothing  -> do
          putTMVar (cfgVar  s) cfg'
          putTMVar (hndlVar s) serial

-- | Get current configuration. Returns 'Just' if
-- the current state is open.
getSportCfg :: Sport -> STM (Maybe SportCfg)
getSportCfg = tryReadTMVar . cfgVar

-- | Close the serial port.
closeSport :: Sport -> IO ()
closeSport s = join $ atomically $ do
  _  <- tryTakeTMVar $ cfgVar  s
  hM <- tryTakeTMVar $ hndlVar s
  return $ mapM_ hClose hM

-- | Read n bytes from the serial port. If the serial port is
-- closed then throw 'SportClosed'. Blocks until all n bytes
-- are available.
readSport :: Sport -> Int -> IO ByteString
readSport s n = do
  h <- atomically $ readTMVar (hndlVar s) `orElse` throwSTM SportClosed
  BS.hGet h n

-- | Read up to n bytes from the serial port. If the serial port
-- is closed then throw 'SportClosed'. Blocks until some of
-- the n bytes are available.
readSomeSport :: Sport -> Int -> IO ByteString
readSomeSport s n = do
  h <- atomically $ readTMVar (hndlVar s) `orElse` throwSTM SportClosed
  BS.fromStrict <$> Strict.hGetSome h n

-- | Write bytes to the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial
-- port is busy handling a concurrent request.
writeSport :: Sport -> ByteString -> IO ()
writeSport s bs = do
  h <- atomically $ readTMVar (hndlVar s) `orElse` throwSTM SportClosed
  BS.hPut h bs

-- | Flush the serial handle. Do nothing if closed.
flushSport :: Sport -> IO ()
flushSport s = do
  hM <- atomically $ tryReadTMVar $ hndlVar s
  mapM_ hFlush hM

toSerialCfg :: SportCfg -> S.SerialCfg
toSerialCfg s =
  S.SerialCfg
    (path s)
    (speed s)
    (byteSize s)
    (parity s)
    (stopBits s)
    (rtimeout s)
    (excl s)

-- | Serial port exceptions
data SportException
    -- | User required an open sport but it was closed.
  = SportClosed
    -- | User attempted to open a serial port that was already open.
  | SportAlreadyOpen String
  deriving (Eq, Read, Show)

instance Exception SportException

-- | Pretty exception rendering
displaySportException :: SportException -> String
displaySportException e = "sport exception: " <> case e of
  SportClosed -> "serial port closed"
  SportAlreadyOpen p -> "serial port already open: " <> p
