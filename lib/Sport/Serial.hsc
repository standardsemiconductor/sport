{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level serial IO
module Sport.Serial
  ( withSerial
  , openSerial
  , SerialCfg(..)
  , Parity(..)
  , StopBits(..)
  , defSerialCfg
  ) where

import Control.Exception
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import System.Posix

-- | Configuration
data SerialCfg = SerialCfg
  { path :: FilePath
  , speed :: BaudRate
  , byteSize :: Int -- ^ number of bits per byte
  , parity :: Maybe Parity
  , stopBits :: StopBits
  , rtimeout :: Maybe Int
  , excl :: Bool -- ^ exclusive
  } deriving (Eq, Show)

-- | Default configuration
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

-- | Acquire bracketed serial handle
withSerial :: SerialCfg -> (Handle -> IO a) -> IO a
withSerial cfg = bracket (openSerial cfg) hClose

-- | Open serial sport handle
openSerial :: SerialCfg -> IO Handle
openSerial cfg =
  bracketOnError (openFd (path cfg) ReadWrite flags) closeFd $ \fd -> do
    when (excl cfg) $ setExcl fd
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

setExcl :: Fd -> IO ()
setExcl fd = ioctl fd #{const TIOCEXCL} nullPtr

ioctl :: Fd -> Int -> Ptr d -> IO ()
ioctl f req =
  throwErrnoIfMinus1_ "ioctl"
    . c_ioctl (fromIntegral f) (fromIntegral req)
    . castPtr

#include <sys/ioctl.h>
foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO  CInt
