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

calulate :: Float -> Float -> Float
calulate years secconds= (secconds / 31557600) / years

ageOn :: Planet -> Float -> Float
ageOn Mercury = calulate 0.2408467
ageOn Venus   = calulate  0.61519726
ageOn Earth   = calulate   1
ageOn Mars    = calulate   1.8808158
ageOn Jupiter = calulate  11.862615
ageOn Saturn  = calulate  29.447498
ageOn Uranus  = calulate  84.016846
ageOn Neptune = calulate  164.79132
