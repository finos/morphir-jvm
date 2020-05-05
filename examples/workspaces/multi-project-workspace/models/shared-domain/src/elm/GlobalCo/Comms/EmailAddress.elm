module EmailAddress exposing (..)


type EmailAddress
    = EmailAddress String


create : String -> Result String EmailAddress
create text =
    let
        len =
            String.length text

        endIdx =
            len - 1
    in
    if String.isEmpty text then
        "An email address cannot be empty" |> Result.Err

    else
        case String.indices "@" text of
            [] ->
                "Invalid email address" |> Result.Err

            idx :: [] ->
                if idx < endIdx then
                    EmailAddress text |> Result.Ok

                else
                    "Invalid email address" |> Result.Err
