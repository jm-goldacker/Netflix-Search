module Main exposing (..)

import Html exposing (Attribute, Html, button, div, section, text, h1, input, select, option, br, b, label)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Html.Attributes exposing (class, name, placeholder, disabled, selected, hidden, autofocus)
import Http
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Svg exposing (svg, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, xlinkHref)
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { query : String
    , sortBy : String
    , sYear : String
    , eYear : String
    , type_ : String
    , results : List Movie
    , errorMsg : String
    , isLoading : Bool
    , information : Information
    , informationModal : Bool
    , specifyModal : Bool
    }

type alias Movie =
    { title : String
    , image : String
    , synopsis : String
    , rating : String
    , mtype : String
    , imdbid : String
    }

type alias Information =
    { imdbrating : String
    , imdbvotes : String
    , genre : String
    , awards : String
    , freshTomatoes : String
    , rottenTomatoes : String
    , tomatoMeter : String
    , runtime : String
    }

defaultInformation : Information
defaultInformation =
    Information "" "" "" "" "" "" "" ""

defaultModel : Model
defaultModel =
    Model "" "Relevance" "1900" "2018" "Any" [] "" False defaultInformation False False

type Msg
    = LoadMovieJson
    | LoadInformationJson String
    | MovieJsonLoaded (Result Http.Error (List Movie))
    | InformationJsonLoaded (Result Http.Error Information)
    | ChangeQuery String
    | ChangeSortBy String
    | ChangeStartYear String
    | ChangeEndYear String
    | ChangeType String
    | CloseinformationModal
    | CloseSpecifyModal
    | KeyDown Int
    | SpecifySearch


-- UPDATE


init : (Model, Cmd Msg)
init =
    (defaultModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      LoadMovieJson ->
        ({model | isLoading = True}, getMovieJson model)

      LoadInformationJson imdbid ->
        (model, getInformationJson imdbid)

      MovieJsonLoaded (Ok results) ->
        ({model | results = results, isLoading = False}, Cmd.none)

      MovieJsonLoaded (Err myError) ->
        ({model | errorMsg = toString myError, isLoading = False}, Cmd.none)

      InformationJsonLoaded (Ok result) ->
        ({model | information = result, informationModal = True}, Cmd.none)

      InformationJsonLoaded (Err myError) ->
        ({model | errorMsg = toString myError}, Cmd.none)

      ChangeQuery newQuery ->
      --Replace empty spaces with "+"
        ({model | query = String.map (\c -> if c == ' ' then '+' else c) newQuery}, Cmd.none)

      ChangeSortBy sortBy ->
        ({model | sortBy = sortBy}, Cmd.none)

      ChangeStartYear newYear ->
        ({model | sYear = newYear}, Cmd.none)

      ChangeEndYear newYear ->
        ({model | eYear = newYear}, Cmd.none)

      ChangeType newType ->
        ({model | type_ = newType}, Cmd.none)

      CloseinformationModal ->
        ({model | informationModal = False}, Cmd.none)

      CloseSpecifyModal ->
        ({model | specifyModal = False}, Cmd.none)

      KeyDown key ->
        if key == 13 then
          update LoadMovieJson model

        else
          (model, Cmd.none)

      SpecifySearch ->
        ({model | specifyModal = True}, Cmd.none)

--translates the apostrophe
getSynopsis : Movie -> String
getSynopsis movie =
  String.join "'" (String.split "&#39;" movie.synopsis)

getMovieRequest : String -> Http.Request (List Movie)
getMovieRequest url =
    Http.request
      { method = "GET"
      , headers = [Http.header "X-Mashape-Key" "Insert your Key here"]
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson decoder1
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
      , expect = Http.expectJson informationDecoder
      , timeout = Nothing
      , withCredentials = False
      }

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

decoder1 : Decoder (List Movie)
decoder1 =
    at ["ITEMS"] (list movieDecoder)

movieDecoder : Decoder Movie
movieDecoder =
    decode Movie
      |> required "title" string
      |> required "image" string
      |> required "synopsis" string
      |> required "rating" string
      |> required "type" string
      |> required "imdbid" string

informationDecoder : Decoder Information
informationDecoder =
    decode Information
      |> required "imdbrating" string
      |> required "imdbvotes" string
      |> required "genre" string
      |> required "awards" string
      |> required "tomatoFresh" string
      |> required "tomatoRotten" string
      |> required "tomatoMeter" string
      |> required "runtime" string

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.Decode.map tagger keyCode)


--VIEW


view : Model -> Html Msg
view model =
  div []
    --Head
    [ section [ class "hero is-dark" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Search Netflix" ]
                ]
            ]
        ]
    --Body
    , section [ class "section" ]
        [ div [ class "container is-fluid" ]
            [ div [ class "container has-text-centered" ]
            --Control
                [ div [ class "control" ]
                    [ input [ class "input is-black", name "query", placeholder "Query", onInput ChangeQuery, autofocus True, onKeyDown KeyDown ] []
                    , div [class "select"]
                        [ select [ onInput ChangeSortBy ]
                            [ --option [ disabled True, selected True, hidden True ] [ text "Sort by" ]
                              option [ selected True ] [ text "Relevance" ]
                            , option [] [ text "Rating" ]
                            , option [] [ text "Title" ]
                            , option [] [ text "Video Type"]
                            ]
                        ]
                    , button [ class "button", onClick SpecifySearch ] [ text "More" ]
                    , if model.isLoading
                      then button [ class "button is-loading", onClick LoadMovieJson, autofocus True ] [ text "Search" ]
                      else button [ class "button", onClick LoadMovieJson, autofocus True ] [ text "Search" ]
                    ]
                ]
            ]
        , br [] []
        --Results
        , div [ class "hero is-light" ]
            [
              div [ class "container" ]
                ([ text (toString (List.length model.results) ++ " results found.")
                ] ++ (List.map getInformation model.results))
            ]
        --informationModal for information
        , if model.informationModal
          then
            div [ class ("modal is-active") ]
              [ div [ class "modal-background" ] []
              , div [ class "modal-card" ]
                  [ section [ class "modal-card-body" ]
                      [ div [] [ b [] [ text "Genre: " ], text model.information.genre ]
                      , div [] [ b [] [ text "Awards: " ], text model.information.awards ]
                      , div [] [ b [] [ text "Runtime: " ], text model.information.runtime ]
                      , div [] [ b [] [ text "IMDB-Rating: " ], text model.information.imdbrating ]
                      , div [] [ b [] [ text "IMDB-Votes: " ], text model.information.imdbvotes ]
                      , div [] [ b [] [ text "Fresh tomatoes: " ], text model.information.freshTomatoes ]
                      , div [] [ b [] [ text "Rotten tomatoes: " ], text model.information.rottenTomatoes ]
                      , div [] [ b [] [ text "Tomatoe-meter: " ], text model.information.tomatoMeter]
                      ]
                  ]
              , button [ onClick (CloseinformationModal), class "modal-close is-large" ] []
              ]
         else div [] []
         , if model.specifyModal
           then
             div [ class ("modal is-active") ]
               [ div [ class "modal-background" ] []
               , div [ class "modal-card" ]
                   [ section [ class "modal-card-body" ]
                       [ div [ class "field" ]
                          [ label [ class "label" ] [ text "Start year" ]
                          , input [ class "input", onInput ChangeStartYear, placeholder model.sYear ] []
                          , label [ class "label" ] [ text "End year" ]
                          , input [ class "input", onInput ChangeEndYear, placeholder model.eYear ] []
                          , label [ class "label" ] [ text "Type" ]
                          , div [ class "select" ]
                              [ select [ onInput ChangeType ]
                                  [ option [ if model.type_ == "Any" then selected True else selected False ] [ text "Any" ]
                                  , option [ if model.type_ == "Movie" then selected True else selected False ] [ text "Movie" ]
                                  , option [ if model.type_ == "Series" then selected True else selected False ] [ text "Series" ]
                                  ]
                              ]
                          , button [ class "button", onClick(CloseSpecifyModal) ] [ text "OK" ]
                          ]
                       ]
                   ]
               ]
          else div [] []
        --Footer
        , div [ class "footer" ]
            [ text model.errorMsg
            ]
        ]
    ]

viewResults : Model -> Html Msg
viewResults model =
  div [ class "columns" ]
    [ div [ class "column is-one-quarter" ]
        (List.map getInformation model.results)
    ]

getInformation : Movie -> Html Msg
getInformation movie =
  div [ class "columns" ]
    [ div [ class "column is-one-quarter" ]
      [
        svg [ width "166", height "233", viewBox "0 0 166 233" ]
          [ image [ x "0", y "0", width "166px", height "233px", xlinkHref movie.image] []
          ]
      ]
    , div [ class "column" ]
      [ div [] [ b [] [ text "Title: " ], text movie.title ]
      , br [] []
      , div [] [ b [] [ text "Synopsis: " ], text (getSynopsis movie) ]
      , br [] []
      , div [] [ b [] [ text "Netflix-Rating: " ], text movie.rating ]
      , div [] [ b [] [ text "Type: " ], text movie.mtype ]
      , div [] [ b [] [ text "IMDB-ID: " ], text movie.imdbid ]
      , br [] []
      , button [ onClick (LoadInformationJson movie.imdbid) ] [ text "more information" ]
      ]
    ]
