module Sport.Sport
  ( Sport
  , newSportIO
  , newSport
  , withSport
  , runSport
  , openSport
  , isOpenSport
  , getSportCfg
  , getOpenSportCfg
  , defSportCfg
  , SportCfg(..)
  , closeSport
  , readSport
  , readSomeSport
  , writeSport
  , SportException(..)
  ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import qualified Sport.Serial as S
import System.IO
import System.Posix

data Sport = Sport
  { state :: TVar State
  , rcmd  :: TMVar RdCmd
  , wcmd  :: TMVar WrCmd
  }

-- | Acquire IO serial port
newSportIO :: IO Sport
newSportIO = atomically newSport

-- | Acquire STM serial port
newSport :: STM Sport
newSport =
  Sport
    <$> newTVar Closed
    <*> newEmptyTMVar
    <*> newEmptyTMVar

data State
  = Closed
  | Opening SportCfg (TMVar (Either SomeException ()))
  | Open SportCfg Handle
  deriving Eq

isOpen :: State -> Bool
isOpen Open{} = True
isOpen _      = False

-- | Check if serial port is open
isOpenSport :: Sport -> STM Bool
isOpenSport = fmap isOpen . readTVar . state

data RdCmd
  = Rd Int (TMVar (Either SomeException ByteString))
  | RdSome Int (TMVar (Either SomeException ByteString))

data WrCmd
  = Wr ByteString (TMVar (Either SomeException ()))

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

-- | Open the serial port. Throw 'SportAlreadyOpen' if the
-- serial port was already opened.
openSport :: Sport -> SportCfg -> IO ()
openSport (Sport s _ _) cfg' = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed        -> writeTVar s $ Opening cfg' res
      Opening cfg _ -> throwSTM $ SportAlreadyOpen $ path cfg
      Open    cfg _ -> throwSTM $ SportAlreadyOpen $ path cfg
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Get current configuration. Returns 'Just' if
-- the current state is opening or open. See also 'getOpenSportCfg'.
getSportCfg :: Sport -> STM (Maybe SportCfg)
getSportCfg (Sport s _ _) = do
  st <- readTVar s
  return $ case st of
    Closed        -> Nothing
    Opening cfg _ -> Just cfg
    Open    cfg _ -> Just cfg

-- | Get current open configuration. Returns 'Just' if
-- the current state is open. See also 'getSportCfg'.
getOpenSportCfg :: Sport -> STM (Maybe SportCfg)
getOpenSportCfg (Sport s _ _) = do
  st <- readTVar s
  return $ case st of
    Closed     -> Nothing
    Opening{}  -> Nothing
    Open cfg _ -> Just cfg

-- | Close the serial port.
closeSport :: Sport -> IO ()
closeSport (Sport s r w) = join $ atomically $ do
  st <- readTVar s
  writeTVar s Closed
  (rM, wM) <- (,) <$> tryTakeTMVar r <*> tryTakeTMVar w
  mapM_ closeRdCmd rM
  mapM_ closeWrCmd wM
  closeState st

closeRdCmd :: RdCmd -> STM ()
closeRdCmd (Rd     _ res) = closeResponse res
closeRdCmd (RdSome _ res) = closeResponse res

closeWrCmd :: WrCmd -> STM ()
closeWrCmd (Wr _ res) = closeResponse res

closeState :: State -> STM (IO ())
closeState Closed          = return $ return ()
closeState (Opening _ res) = closeResponse res $> return ()
closeState (Open _ serial) = return $ hClose serial

closeResponse :: TMVar (Either SomeException a) -> STM ()
closeResponse res = void $ tryPutTMVar res $ Left $ toException SportClosed

-- | Read n bytes from the serial port. If the serial port is
-- closed then throw 'SportClosed'. Block if the serial port is
-- busy handling a concurrent request or until all n bytes are available.
readSport :: Sport -> Int -> IO ByteString
readSport (Sport s r _) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed -> throwSTM SportClosed
      Open{} -> putTMVar r $ Rd n res
      _      -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Read up to n bytes from the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial port
-- is busy handling a concurrent request or until some of
-- the n bytes are available.
readSomeSport :: Sport -> Int -> IO ByteString
readSomeSport (Sport s r _) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed -> throwSTM SportClosed
      Open{} -> putTMVar r $ RdSome n res
      _      -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Write bytes to the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial
-- port is busy handling a concurrent request.
writeSport :: Sport -> ByteString -> IO ()
writeSport (Sport s _ w) bs = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed -> throwSTM SportClosed
      Open{} -> putTMVar w $ Wr bs res
      _      -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Acquire a serial port and run the daemon.
withSport :: (Sport -> IO a) -> IO a
withSport k = do
  s <- newSportIO
  either id id <$> race (k s) (runSport s)

-- | Run the serial port daemon and process requests.
runSport :: Sport -> IO a
runSport s =
  forever (runState s =<< readTVarIO (state s)) `onException` closeSport s

runState :: Sport -> State -> IO ()
runState s st = case st of
  Closed -> waitNewState s st
  Opening cfg res ->
    handleException res $
      opening s cfg res
        `onException` atomically (writeTVar (state s) Closed)
  Open _ serial -> runConcurrently $ asum $ map Concurrently
    [ waitNewState s st
    , readHandler s serial
    , writeHandler s serial
    ]

readHandler :: Sport -> Handle -> IO a
readHandler s serial = forever $ do
  cmd <- atomically $ takeTMVar $ rcmd s
  case cmd of
    Rd n res -> handleException res $ do
      bs <- BS.hGet serial n
      atomically $ writeTMVar res $ Right bs
    RdSome n res -> handleException res $ do
      bs <- BS.fromStrict <$> Strict.hGetSome serial n
      atomically $ writeTMVar res $ Right bs

writeHandler :: Sport -> Handle -> IO a
writeHandler s serial = forever $ do
  cmd <- atomically $ takeTMVar $ wcmd s
  case cmd of
    Wr bs res -> handleException res $ do
      BS.hPut serial bs
      atomically $ writeTMVar res $ Right ()

waitNewState :: Sport -> State -> IO ()
waitNewState s st = atomically $ check . (st /=) =<< readTVar (state s)

opening :: Sport -> SportCfg -> TMVar (Either SomeException ()) -> IO ()
opening s cfg res =
  bracketOnError (S.openSerial $ toSerialCfg cfg) hClose $ \serial -> do
    hSetBinaryMode serial $ binaryMode cfg
    hSetBuffering serial $ bufferMode cfg
    atomically $ do
      writeTVar (state s) $ Open cfg serial
      writeTMVar res $ Right ()

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
  | SportAlreadyOpen String
  deriving Eq

instance Show SportException where
  show e = "sport exception: " <> case e of
    SportClosed -> "serial port closed"
    SportAlreadyOpen p -> "serial port already open: " <> p

instance Exception SportException
