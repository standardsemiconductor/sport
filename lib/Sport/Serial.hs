module Sport.Serial
  ( withSerial
  , openSerial
  , SerialCfg(..)
  , Parity(..)
  , StopBits(..)
  , defSerialCfg
  ) where

import Control.Exception
import System.IO
import System.Posix

data SerialCfg = SerialCfg
  { path :: FilePath
  , speed :: BaudRate
  , byteSize :: Int -- ^ number of bits per byte
  , parity :: Maybe Parity
  , stopBits :: StopBits
  , rtimeout :: Maybe Int
  , excl :: Bool -- ^ exclusive
  } deriving (Eq, Show)

defSerialCfg :: SerialCfg
defSerialCfg = SerialCfg
  { path = "/dev/ttyUSB0"
  , speed = B115200
  , byteSize = 8
  , parity = Nothing
  , stopBits = One
  , rtimeout = Nothing
  , excl = True
  }

data Parity = Even | Odd
  deriving (Eq, Read, Show)

data StopBits = One | Two
  deriving (Eq, Read, Show)

withSerial :: SerialCfg -> (Handle -> IO a) -> IO a
withSerial cfg = bracket (openSerial cfg) hClose

openSerial :: SerialCfg -> IO Handle
openSerial cfg =
  bracketOnError (openFd (path cfg) ReadWrite flags) closeFd $ \fd -> do
    configAttrs fd cfg
    fdToHandle fd
  where
    flags =
      defaultFileFlags
        { noctty    = True
        , nonBlock  = True
        , exclusive = excl cfg
        }

-- | configure FD terminal attributes
configAttrs :: Fd -> SerialCfg -> IO ()
configAttrs fd cfg = do
  attrs <- getTerminalAttributes fd
  setTerminalAttributes fd (config attrs) Immediately
  where
    config attrs = attrs
      `withInputSpeed` speed cfg
      `withOutputSpeed` speed cfg
      `withBits` byteSize cfg
      `withParity` parity cfg
      `withStopBits` stopBits cfg
      `withoutMode` StartStopInput
      `withoutMode` StartStopOutput
      `withoutMode` EnableEcho
      `withoutMode` EchoErase
      `withoutMode` EchoKill
      `withoutMode` ProcessInput
      `withoutMode` ProcessOutput
      `withoutMode` MapCRtoLF
      `withoutMode` EchoLF
      `withoutMode` HangupOnClose
      `withoutMode` KeyboardInterrupts
      `withoutMode` ExtendedFunctions
      `withMode` LocalMode
      `withMode` ReadEnable
      `withReadTimeout` rtimeout cfg
      `withMinInput` 0

withParity :: TerminalAttributes -> Maybe Parity -> TerminalAttributes
withParity attrs Nothing  = attrs `withoutMode` EnableParity
withParity attrs (Just p) = case p of
  Even -> attrs `withoutMode` OddParity
  Odd  -> attrs `withMode`    OddParity

withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits attrs One = attrs `withoutMode` TwoStopBits
withStopBits attrs Two = attrs `withMode`    TwoStopBits

withReadTimeout :: TerminalAttributes -> Maybe Int -> TerminalAttributes
withReadTimeout attrs Nothing  = attrs
withReadTimeout attrs (Just t) = attrs `withTime` t
