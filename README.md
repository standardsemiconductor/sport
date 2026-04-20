# Sport

UNIX Serial Port

```hs
import Sport

main = withSport $ \s -> do
  openSport s defSportCfg{path="/dev/ttyUSB1"}
  bs <- readSport s 64
  writeSport s bs
```

Development

```
cabal build
cabal haddock
```
