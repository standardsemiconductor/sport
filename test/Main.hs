module Main (main) where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BS
import Sport
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "SPORT TEST"
  s <- newSportIO
  putStr "OPENING..."
  openSport s defSportCfg{speed=B19200}
  putStrLn "DONE"
  let bs = BS.replicate 256 0xDC
  putStr "XFERING..."
  bs' <- fst <$> concurrently (readSport s 10) (writeSport s bs)
  putStrLn "DONE"
  putStr "COMPARING..."
  putStrLn $ if BS.take 10 bs == bs' then "PASS" else "FAIL"
  putStr "CLOSING..."
  closeSport s
  putStrLn "DONE"
