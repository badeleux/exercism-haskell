module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYInSec = 31557600 

ageOn :: Planet -> Float -> Float
ageOn Earth s =  s / earthYInSec
ageOn Mercury s =  s / earthYInSec / 0.2408467 
ageOn Venus s =  s / earthYInSec / 0.61519726
ageOn Mars s =  s / earthYInSec / 1.8808158 
ageOn Jupiter s =  s / earthYInSec / 11.862615
ageOn Saturn s =  s / earthYInSec / 29.447498
ageOn Uranus s =  s / earthYInSec / 84.016846
ageOn Neptune s =  s / earthYInSec / 164.79132 
