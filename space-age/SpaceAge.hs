module SpaceAge where

import Data.Maybe (fromJust)

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune deriving (Show, Eq)

ageOn :: Planet -> Integer -> Double
ageOn p s = (fromIntegral s) / 31557600.0 / (fromJust $ lookup p planets) where
    planets = [(Mercury, 0.2408467), (Venus, 0.61519726), (Earth, 1.0), (Mars, 1.8808158), (Jupiter, 11.862615), (Saturn, 29.447498), (Uranus, 84.016846), (Neptune, 164.79132)]