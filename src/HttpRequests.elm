module HttpRequests exposing (getMovieJson, getInformationJson)

import Http
import Types exposing (Model, Movie, Information, Msg(..))
import Decoders


getMovieJson : Model -> Cmd Msg
getMovieJson model =
    let
        url =
          "https://unogs-unogs-v1.p.mashape.com/aaapi.cgi?q="
          ++ model.query
          ++ "-!"
          ++ model.sYear
          ++ ","
          ++ model.eYear
          ++ "-!0,5-!0,10-!0-!"
          ++ model.type_
          ++ "-!Any-!Any-!gt100-!{downloadable}&t=ns&cl=all&st=adv&ob="
          ++ model.sortBy
          ++ "&p=1&sa=and"
    in
        Http.send MovieJsonLoaded (getMovieRequest url)

getInformationJson : String -> Cmd Msg
getInformationJson imdbid =
    let
        url =
          "https://unogs-unogs-v1.p.mashape.com/aaapi.cgi?t=getimdb&q="
          ++ imdbid
    in
        Http.send InformationJsonLoaded (getInformationRequest url)

getMovieRequest : String -> Http.Request (List Movie)
getMovieRequest url =
    Http.request
      { method = "GET"
      , headers = [Http.header "X-Mashape-Key" "Insert your Key here"]
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson Decoders.jsonDecoder
      , timeout = Nothing
      , withCredentials = False
      }

getInformationRequest : String -> Http.Request Information
getInformationRequest url =
    Http.request
      { method = "GET"
      , headers = [Http.header "X-Mashape-Key" "Insert your Key here"]
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson Decoders.informationDecoder
      , timeout = Nothing
      , withCredentials = False
      }