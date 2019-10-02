module View exposing (view)

import Html exposing (Attribute, Html, button, div, section, text, h1, input, select, option, br, b, label)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Html.Attributes exposing (class, name, placeholder, disabled, selected, hidden, autofocus)
import Svg exposing (svg, image)
import Svg.Attributes exposing (width, height, viewBox, x, y, xlinkHref)
import Types exposing (Model, Movie, Information, Msg (..))
import Json.Decode

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

getSynopsis : Movie -> String
getSynopsis movie =
  String.join "'" (String.split "&#39;" movie.synopsis)

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.Decode.map tagger keyCode)