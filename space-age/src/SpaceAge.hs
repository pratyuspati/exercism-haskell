module SpaceAge (Planet(..), ageOn) where

type Age = Float

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
  deriving (Eq)

toYears :: Float -> Age
toYears sec = sec / ( 60 * 60 * 24 * 365.25 )

ageOn :: Planet -> Float -> Age
ageOn planet age =
  case planet of
       Mercury -> toYears age / 0.2408467
       Venus   -> toYears age / 0.61519726
       Earth   -> toYears age / 1.0
       Mars    -> toYears age / 1.8808158
       Jupiter -> toYears age / 11.862615
       Saturn  -> toYears age / 29.447498
       Uranus  -> toYears age / 84.016846
       Neptune -> toYears age / 164.79132
