module Gigasecond where 

import Data.Time.Clock (UTCTime, addUTCTime, NominalDiffTime)

fromDay :: UTCTime -> UTCTime
fromDay a = addUTCTime (1000000000 :: NominalDiffTime) a
