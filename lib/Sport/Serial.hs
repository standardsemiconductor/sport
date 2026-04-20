module Sport.Serial
  ( withSerial
  , openSerial
  , SerialConfig(..)
  , Parity(..)
  , StopBits(..)
  , defSerialConfig
  ) where

import Control.Exception
import System.IO
import System.Posix

data SerialConfig = SerialConfig
  { path :: FilePath
  , speed :: BaudRate
  , byteSize :: Int -- ^ number of bits per byte
  , parity :: Maybe Parity
  , stopBits :: StopBits
  , rtimeout :: Maybe Int
  , excl :: Bool -- ^ exclusive
  } deriving (Eq, Show)

defSerialConfig :: SerialConfig
defSerialConfig = SerialConfig
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

withSerial :: SerialConfig -> (Handle -> IO a) -> IO a
withSerial settings k =
  bracket (openSerialFd settings) closeFd $ \fd ->
  bracket (fdToHandle fd) hClose k

openSerial :: SerialConfig -> IO Handle
openSerial cfg = fdToHandle =<< openSerialFd cfg

openSerialFd :: SerialConfig -> IO Fd
openSerialFd cfg = do
  fd <- openFd (path cfg) ReadWrite flags
  attrs <- getTerminalAttributes fd
  setTerminalAttributes fd (attrs `withConfig` cfg) Immediately
  return fd
  where
    flags =
      defaultFileFlags
        { noctty    = True
        , nonBlock  = True
        , exclusive = excl cfg
        }

withConfig :: TerminalAttributes -> SerialConfig -> TerminalAttributes
withConfig attrs cfg =
  attrs
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
