module GlobalCo.Comms.CustomerEmail exposing (CustomerEmail(..), VerifiedEmailAddress)

import EmailAddress exposing (EmailAddress)


type VerifiedEmailAddress
    = VerifiedEmailAddress String


type CustomerEmail
    = Unverified EmailAddress
    | Verified VerifiedEmailAddress
