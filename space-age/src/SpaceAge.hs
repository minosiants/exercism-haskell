module SpaceAge
  ( Planet(..)
  , ageOn
  ) where

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

calculate :: Float -> Float -> Float
calculate years secconds= (secconds / 31557600) / years

ageOn :: Planet -> Float -> Float
ageOn Mercury = calculate 0.2408467
ageOn Venus   = calculate  0.61519726
ageOn Earth   = calculate   1
ageOn Mars    = calculate   1.8808158
ageOn Jupiter = calculate  11.862615
ageOn Saturn  = calculate  29.447498
ageOn Uranus  = calculate  84.016846
ageOn Neptune = calculate  164.79132
