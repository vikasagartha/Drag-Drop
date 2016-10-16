module ImageSearch exposing (Model, init, Msg (..), update, view)

import Html.App
import Html.Events
import Html.Attributes
import Html
import Json.Decode
import Http
import Task

type alias Model =
    { results : List PixabayImage
    }

init : Model
init =
    { results = [] }

type Msg 
    = DoSearch
    | SearchFailure Http.Error
    | SearchSuccess PixabaySearchResponse 
    | ImageSelected { url: String }

type alias PixabaySearchResponse = 
    { totalHits : Int
    , hits: List PixabayImage
    }

pixabaySearchResponseDecoder : Json.Decode.Decoder PixabaySearchResponse
pixabaySearchResponseDecoder =
    Json.Decode.object2 PixabaySearchResponse
        (Json.Decode.at ["totalHits"] Json.Decode.int)
        (Json.Decode.at ["hits"] (Json.Decode.list pixabayImageDecoder))

type alias PixabayImage = 
    { preview : String
    , webFormat : String
    }

pixabayImageDecoder : Json.Decode.Decoder PixabayImage
pixabayImageDecoder = 
    Json.Decode.object2 PixabayImage
        (Json.Decode.at ["previewURL"] Json.Decode.string)
        (Json.Decode.at ["webformatURL"] Json.Decode.string)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "msg" msg of
        DoSearch ->
            ( model
            , Http.get pixabaySearchResponseDecoder "https://pixabay.com/api/?key=3339667-7100c76194672f93f7ed803eb&q=yellow+flowers&image_type=photo&pretty=true" 
                |> Task.perform SearchFailure SearchSuccess
            ) 
        SearchSuccess data ->
            ( { model | results = data.hits } 
            , Cmd.none 
            )
        SearchFailure error ->
            ( model, Cmd.none )
        ImageSelected _ ->
            ( model, Cmd.none )

view : Model -> Html.Html Msg
view model = 
    Html.div []
    [ Html.button 
        [ Html.Events.onClick DoSearch ] 
        [ Html.text "Search" ]
    , Html.ul [] (List.map viewImage model.results) 
    ]

viewImage : PixabayImage -> Html.Html Msg
viewImage image =
    Html.li []
    [ Html.img 
        [ Html.Attributes.style 
            [ ("max-width", "100px")
            , ("max-height", "100px")
            ]
        , Html.Attributes.src image.preview
        , Html.Events.onClick (ImageSelected { url = image.webFormat })
        ]
        []
    ]

subscriptions model =
    Sub.none

main = Html.App.program
    { init = (init, Cmd.none)
    , subscriptions = subscriptions 
    , update =  update 
    , view =  view 
    }
