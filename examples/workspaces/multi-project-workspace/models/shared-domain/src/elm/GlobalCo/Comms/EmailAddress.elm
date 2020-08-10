{-
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}


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
