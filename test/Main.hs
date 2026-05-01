module Main (main) where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BS
import Sport

main :: IO ()
main = do
  s <- newSportIO
  race_ (runSport s) $ do
    openSport s defSportCfg{speed=B19200}
    let bs = BS.replicate 256 0xDC
    writeSport s bs
    bs' <- readSport s 256
    putStrLn $ if bs == bs' then "PASS" else "FAIL"
    closeSport s
