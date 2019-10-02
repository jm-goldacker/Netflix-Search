module Types exposing (Model, defaultModel, Information, Msg (..), Movie)

import Http

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

defaultModel : Model
defaultModel =
    Model "" "Relevance" "1900" "2018" "Any" [] "" False defaultInformation False False

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