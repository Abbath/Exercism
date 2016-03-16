module Gigasecond where 

import Data.Time.Clock (UTCTime, addUTCTime, NominalDiffTime)

fromDay :: UTCTime -> UTCTime
fromDay a = addUTCTime (1e9 :: NominalDiffTime) a
