module Update exposing (update)

import HttpRequests
import Types exposing (Model, Msg (..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      LoadMovieJson ->
        ({model | isLoading = True}, HttpRequests.getMovieJson model)

      LoadInformationJson imdbid ->
        (model, HttpRequests.getInformationJson imdbid)

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