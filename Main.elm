module Main exposing (..)

import Html
import Html.App
import Html.Attributes


--Init 
type alias Model = ()

initialModel : Model
initialModel =
    ()

--Update
type Msg = NothingYet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

--View
viewCanvas : Html.Html Msg
viewCanvas =
    Html.div 
        [ Html.Attributes.style 
            [ ("width", "250px") 
            , ("height", "250px") 
            , ("border", "2px solid black") 
            ]
        ]
        [ Html.div
            [ Html.Attributes.style 
                [ ("height", "250px") 
                , ("background-image", "url(http://i.imgur.com/bjjypBA.jpg)") 
                , ("background-size", "auto 250px") 
                ]
            ]
            []
        ]

view : Model -> Html.Html Msg
view model =
    Html.div 
        [ Html.Attributes.style 
            [ ("padding", "8px") 
            ]
        ]
        [ viewCanvas
        , Html.hr [] []
        , Html.text <| toString model 
        ]

subscriptions =
    Sub.none

main = Html.App.program
    { init = (initialModel, Cmd.none)
    , subscriptions = \_ -> Sub.none
    , update =  update 
    , view =  view 
    }
