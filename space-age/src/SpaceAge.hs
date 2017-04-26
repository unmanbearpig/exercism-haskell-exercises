module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYears :: Float -> Float
earthYears years = years * (planetYearInSeconds Earth)

planetYearInSeconds :: Planet -> Float
planetYearInSeconds planet = case planet of
   Earth    -> 31557600
   Mercury  -> earthYears 0.2408467
   Venus    -> earthYears 0.61519726
   Mars     -> earthYears 1.8808158
   Jupiter  -> earthYears 11.862615
   Saturn   -> earthYears 29.447498
   Uranus   -> earthYears 84.016846
   Neptune  -> earthYears 164.79132

   _ ->  1
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (planetYearInSeconds planet)
