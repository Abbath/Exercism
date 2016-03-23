{-# LANGUAGE TemplateHaskell #-}
module Person where

import           Data.Time.Calendar
import           Control.Lens

data Person = Person {
      _name    :: Name,
      _born    :: Born,
      _address :: Address
    }

data Name = Name {
      _foreNames :: String, -- Space separated
      _surName   :: String
    }

data Born = Born {
      _bornAt :: Address,
      _bornOn :: Day
    }

data Address = Address {
      _street      :: String,
      _houseNumber :: Int,
      _place       :: String, -- Village / city
      _country     :: String
    }

-- Valid values of Gregorian are those for which 'Data.Time.Calendar.fromGregorianValid'
-- returns Just.
data Gregorian = Gregorian {
      _year  :: Integer,
      _month :: Int,
      _day   :: Int
    }

makeLenses ''Name
makeLenses ''Born
makeLenses ''Address
makeLenses ''Gregorian
makeLenses ''Person

-- Implement these.

bornStreet :: Born -> String
bornStreet = (^.bornAt.street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address.street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth m p = set (born.bornOn) mm p
    where
        mm = case uncurry3 fromGregorianValid $ ((\(Gregorian y m d) -> (y,m,d)) (set month m g)) of
                  Just d -> d
                  Nothing -> error "Bad month!"
        g = (\(y,m,d) -> Gregorian y m d) . toGregorian $ p ^. (born.bornOn) 

-- | Transform both birth and current street names.
renameStreets :: (String -> String) -> Person -> Person
renameStreets f p = over (born.bornAt.street) f (over (address.street) f p)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c