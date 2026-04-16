# UNIX Serial Port

```hs
import Sport

main = withSport $ \\s -> do
  openSport s defSerialConfig
  bs <- readSport s 64
  writeSport s bs
```
