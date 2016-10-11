module Main exposing (..)

import Html
import Html.App
import Html.Attributes

type alias Size = 
    { width : Int, height : Int }

type Frame
    = SingleImage { url : String }
    | HorizontalSplit 
        { top : Frame
        , bottom : Frame
        , topHeight : Int
        }

type alias Model =
    { canvas : Size, frame : Frame }

--Init 

initialModel : Model
initialModel =
    { canvas = 
        { width = 250
        , height = 250 
        }
    , frame = 
        HorizontalSplit 
            { top = SingleImage  { url = "http://i.imgur.com/bjjypBA.jpg" }
            , bottom = SingleImage { url = "http://i.imgur.com/K02jg2O.jpg" }
            , topHeight = 80
            }
    }

--Update
type Msg = NothingYet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

--View

viewFrame : Size -> Frame -> Html.Html Msg
viewFrame size frame = 
    case frame of 
        SingleImage { url } ->
            Html.div
                [ Html.Attributes.style 
                    [ ("height", toString size.height ++ "px")
                    , ("width", toString size.width ++ "px")
                    , ("background-image", "url("++url++")") 
                    , ("background-size", "auto " ++ toString size.height ++ "px")
                    ]
                ]
                []
        HorizontalSplit { top, topHeight, bottom } ->
            Html.div []
            [ viewFrame { width = size.width, height = topHeight } top
            , viewFrame { width = size.width, height = size.height - topHeight } bottom
            ]


viewCanvas : Size -> Frame -> Html.Html Msg
viewCanvas size rootFrame =
    Html.div 
        [ Html.Attributes.style 
            [ ("width", toString size.width ++ "px") 
            , ("height", toString size.height ++ "px") 
            , ("border", "2px solid black") 
            ]
        ]
        [ viewFrame size rootFrame 
        ]

view : Model -> Html.Html Msg
view model =
    Html.div 
        [ Html.Attributes.style 
            [ ("padding", "8px") 
            ]
        ]
        [ viewCanvas model.canvas model.frame
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
