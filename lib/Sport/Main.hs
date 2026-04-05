module Sport.Main (sportMain) where

import Control.Concurrent.Async
import Control.Monad
import Sport.Serial
import System.IO
import System.Posix

sportMain :: IO ()
sportMain = do
  putStrLn "...UNIX.SERIAL.PORT..."
  withSerial defSerialConfig{speed = B19200} $ \serial -> do
    hSetBuffering serial NoBuffering
    concurrently_ (reading serial) (writing serial)

reading :: Handle -> IO a
reading serial = forever $ hGetChar serial >>= putChar

writing :: Handle -> IO a
writing serial = forever $ getChar >>= hPutChar serial
