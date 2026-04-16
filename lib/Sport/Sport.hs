module Sport.Sport
  ( Sport
  , newSport
  , newSportSTM
  , withSport
  , runSport
  , openSport
  , closeSport
  , readSport
  , readSomeSport
  , writeSport
  , SportException(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Sport.Serial
import System.IO

data Sport = Sport (TVar State)

-- | Acquire IO serial port
newSport :: IO Sport
newSport = atomically newSportSTM

-- | Acquire STM serial port
newSportSTM :: STM Sport
newSportSTM = Sport <$> newTVar Closed

data State
  = Closed
  | Opening SerialConfig (TMVar (Either SomeException ()))
  | Open SerialConfig Handle
  | Rd SerialConfig Handle Int (TMVar (Either SomeException ByteString))
  | RdSome SerialConfig Handle Int (TMVar (Either SomeException Strict.ByteString))
  | Wr SerialConfig Handle ByteString (TMVar (Either SomeException ()))
  deriving Eq

-- | Open the serial port. Throw 'SportAlreadyOpen' if the
-- serial port was already opened.
openSport :: Sport -> SerialConfig -> IO ()
openSport (Sport s) cfg = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed -> writeTVar s $ Opening cfg res
      _      -> throwSTM SportAlreadyOpen
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Close the serial port.
closeSport :: Sport -> IO ()
closeSport (Sport s) = join $ atomically $ do
  st <- readTVar s
  writeTVar s Closed
  case st of
    Closed                 -> return $ return ()
    Opening _          res -> closeResponse res $> return ()
    Open    _ serial       -> return $ hClose serial
    Rd      _ serial _ res -> closeResponse res $> hClose serial
    RdSome  _ serial _ res -> closeResponse res $> hClose serial
    Wr      _ serial _ res -> closeResponse res $> hClose serial

closeResponse :: TMVar (Either SomeException a) -> STM ()
closeResponse res = void $ tryPutTMVar res $ Left $ toException SportClosed

-- | Read n bytes from the serial port. If the serial port is
-- closed then throw 'SportClosed'. Block if the serial port is
-- busy handling a concurrent request or until all n bytes are available.
readSport :: Sport -> Int -> IO ByteString
readSport (Sport s) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ Rd cfg serial n res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Read up to n bytes from the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial port
-- is busy handling a concurrent request or until some of
-- the n bytes are available.
readSomeSport :: Sport -> Int -> IO Strict.ByteString
readSomeSport (Sport s) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ RdSome cfg serial n res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Write bytes to the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial
-- port is busy handling a concurrent request.
writeSport :: Sport -> ByteString -> IO ()
writeSport (Sport s) bs = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ Wr cfg serial bs res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Acquire a serial port and run the daemon.
withSport :: (Sport -> IO a) -> IO a
withSport k = do
  s <- newSport
  either id id <$> race (k s) (runSport s)

-- | Run the serial port daemon and process requests.
runSport :: Sport -> IO a
runSport sp@(Sport s) =
  forever (runState s =<< readTVarIO s) `onException` closeSport sp

runState :: TVar State -> State -> IO ()
runState s st = case st of
  Closed                    -> waitNewState s st
  Opening cfg           res -> handleException res $ opening s cfg res
  Open{}                    -> waitNewState s st
  Rd      cfg serial n  res -> handleException res $ reading s cfg serial n res
  RdSome  cfg serial n  res -> handleException res $ readingSome s cfg serial n res
  Wr      cfg serial bs res -> handleException res $ writing s cfg serial bs res

waitNewState :: TVar State -> State -> IO ()
waitNewState s st = atomically $ check . (st /=) =<< readTVar s

opening :: TVar State -> SerialConfig -> TMVar (Either SomeException ()) -> IO ()
opening s cfg res =
  bracketOnError (openSerial cfg) hClose $ \serial ->
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right ()

reading
  :: TVar State
  -> SerialConfig
  -> Handle
  -> Int
  -> TMVar (Either SomeException ByteString)
  -> IO ()
reading s cfg serial n res = do
  bs <- BS.hGet serial n
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right bs

readingSome
  :: TVar State
  -> SerialConfig
  -> Handle
  -> Int
  -> TMVar (Either SomeException Strict.ByteString)
  -> IO ()
readingSome s cfg serial n res = do
  bs <- Strict.hGetSome serial n
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right bs

writing
  :: TVar State
  -> SerialConfig
  -> Handle
  -> ByteString
  -> TMVar (Either SomeException ())
  -> IO ()
writing s cfg serial bs res = do
  BS.hPut serial bs
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right ()

handleException :: TMVar (Either SomeException a) -> IO () -> IO ()
handleException res k = k `catches`
  [ Handler $ \e -> throwIO (e :: SomeAsyncException)
  , Handler $ \e -> atomically $ void $ tryPutTMVar res $ Left (e :: SomeException)
  ]

-- | Serial port exceptions
data SportException
    -- | User required an open sport but it was closed.
  = SportClosed
    -- | User attempted to open a serial port that was already open.
  | SportAlreadyOpen
  deriving (Eq, Read, Show)

instance Exception SportException
