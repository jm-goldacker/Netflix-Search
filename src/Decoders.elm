module Decoders exposing (jsonDecoder, informationDecoder)

import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode as Decode
import Types exposing (Movie, Information)

jsonDecoder : Decode.Decoder (List Movie)
jsonDecoder =
    Decode.at ["ITEMS"] (Decode.list movieDecoder)

movieDecoder : Decode.Decoder Movie
movieDecoder =
    decode Movie
      |> required "title" Decode.string
      |> required "image" Decode.string
      |> required "synopsis" Decode.string
      |> required "rating" Decode.string
      |> required "type" Decode.string
      |> required "imdbid" Decode.string

informationDecoder : Decode.Decoder Information
informationDecoder =
    decode Information
      |> required "imdbrating" Decode.string
      |> required "imdbvotes" Decode.string
      |> required "genre" Decode.string
      |> required "awards" Decode.string
      |> required "tomatoFresh" Decode.string
      |> required "tomatoRotten" Decode.string
      |> required "tomatoMeter" Decode.string
      |> required "runtime" Decode.string