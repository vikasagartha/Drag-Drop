module Main exposing (..)

import Html
import Html.App
import Html.Attributes
import Html.Events
import Json.Decode
import Mouse 

borderColor : String
borderColor =
    "tan"

type alias Size = 
    { width : Int, height : Int }

type alias Position= 
    { x : Int, y : Int }

type alias Image = 
    { url : String
    , size : Size
    , offset : Position
    }

type Frame
    = SingleImage Image 
    | HorizontalSplit 
        { top : Frame
        , bottom : Frame
        , topHeight : Int
        }

type alias Model =
    { canvas : Size
    , frame : Frame 
    , borderSize : Int
    , dragSliderState : Maybe Mouse.Position
    , dragImageState : Maybe Mouse.Position
    }

--Init 

initialModel : Model
initialModel =
    { canvas = 
        { width = 250
        , height = 250
        }
    , borderSize = 5
    , frame = 
        --HorizontalSplit 
        --    { top = 
                SingleImage  
                    { url = "http://i.imgur.com/bjjypBA.jpg"
                    , size = { width = 960, height = 637 } 
                    , offset = { x = 0, y = 0} 
                    }
     --       , bottom = 
     --           SingleImage 
     --               { url = "http://i.imgur.com/K02jg2O.jpg" 
     --               , size = { width = 960, height = 618 } 
     --               , offset = { x = 0, y = 0} 
     --               }
     --       , topHeight = 80
     --       }
    , dragSliderState = Nothing 
    , dragImageState = Nothing 
    }

--Update
type Msg 
    = DragDividerStart Mouse.Position
    | DragImageStart Mouse.Position
    | DragMove Mouse.Position
    | DragEnd Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "msg" msg of
        DragDividerStart position ->
            ( { model | dragSliderState = Just position }
            , Cmd.none
            )

        DragImageStart position ->
            ( { model | dragImageState = Just position }
            , Cmd.none
            )

        DragMove currentPosition ->
            case model.dragSliderState of
                Just startPosition ->
                    ( { model 
                        | frame = 
                            applyDrag 
                                (currentPosition.y - startPosition.y) 
                                model.frame 
                        , dragSliderState = Just currentPosition
                      }
                    , Cmd.none
                    )
                Nothing ->
                    case model.dragImageState of
                        Just startPosition ->
                            ( { model 
                                | frame =
                                    applyImageDrag 
                                        { x = startPosition.x - currentPosition.x
                                        , y = startPosition.y - currentPosition.y
                                        }
                                        model.frame
                                , dragImageState = Just currentPosition
                              }
                            , Cmd.none
                            )
                        Nothing ->
                            (model, Cmd.none)

        DragEnd endPosition ->
            ( { model 
                | dragSliderState = Nothing
                , dragImageState = Nothing 
                }
            , Cmd.none
            )

applyDrag : Int -> Frame -> Frame
applyDrag yChange frame =
    case frame of
        HorizontalSplit {top, topHeight, bottom} ->
            HorizontalSplit 
                { top = top
                , bottom = bottom
                , topHeight = topHeight + yChange
                }
        SingleImage _ ->
            frame

applyImageDrag : Position -> Frame -> Frame
applyImageDrag change frame =
    case frame of
        SingleImage image ->
            SingleImage 
                { image 
                    | offset = 
                        { x = image.offset.x + change.x 
                        , y = image.offset.y + change.y 
                        }
                }
        HorizontalSplit _ ->
            frame

--View

--960 x 637 beetle (1.507)
-- 250 x 80 frame  (3.125)

viewFrame : Int -> Size -> Frame -> Html.Html Msg
viewFrame borderSize size frame = 
    case frame of 
        SingleImage image ->
            let 
                imageRatio = 
                    toFloat image.size.width / toFloat image.size.height
                frameRatio = 
                    toFloat size.width / toFloat size.height
            in
                Html.div
                    [ Html.Attributes.style 
                        [ ("height", toString size.height ++ "px")
                        , ("width", toString size.width ++ "px")
                        , ("background-image", "url("++image.url++")") 
                        , ("background-size", 
                            if imageRatio > frameRatio then 
                                "auto " ++ toString size.height ++ "px"
                            else 
                                toString size.width ++ "px auto"
                          )
                        , ( "background-position"
                            , toString -image.offset.x ++"px " ++ toString -image.offset.y ++"px"
                          )
                        ]
                    , Html.Events.on "mousedown" 
                        (Json.Decode.map DragImageStart Mouse.position)
                    ]
                    []
        HorizontalSplit { top, topHeight, bottom } ->
            Html.div []
            [ viewFrame borderSize { width = size.width, height = topHeight } top
            , Html.div 
                [ Html.Attributes.style
                    [ ("width", toString size.width ++ "px")
                    , ("height", toString borderSize ++ "px")
                    , ("background-color", borderColor)
                    , ("cursor", "ns-resize")
                    ]
                , Html.Events.on "mousedown" 
                    (Json.Decode.map DragDividerStart Mouse.position)
                ]
                []
            , viewFrame borderSize { width = size.width, height = size.height - topHeight - borderSize } bottom
            ]


viewCanvas : Int -> Size -> Frame -> Html.Html Msg
viewCanvas borderSize size rootFrame =
    Html.div 
        [ Html.Attributes.style 
            [ ("width", toString size.width ++ "px") 
            , ("height", toString size.height ++ "px") 
            , ("border", "2px solid black") 
            ]
        ]
        [ Html.div 
            [ Html.Attributes.style 
                [ ("border", toString borderSize ++ "px solid tan")
                ]
            ]
            [viewFrame 
                borderSize
                { width = size.width - 2*borderSize
                , height = size.width - 2*borderSize
                }
                rootFrame
            ]
        ]

view : Model -> Html.Html Msg
view model =
    Html.div 
        [ Html.Attributes.style 
            [ ("padding", "8px") 
            ]
        ]
        [ viewCanvas model.borderSize model.canvas model.frame
        , Html.hr [] []
        , Html.text <| toString model 
        ]

subscriptions model =
    case (model.dragSliderState, model.dragImageState) of
        (Just _, _) ->
            Sub.batch 
                [ Mouse.moves DragMove
                , Mouse.ups DragEnd     
                ]
        (_, Just _) ->
            Sub.batch 
                [ Mouse.moves DragMove
                , Mouse.ups DragEnd     
                ]
        _ ->
            Sub.none

main = Html.App.program
    { init = (initialModel, Cmd.none)
    , subscriptions = subscriptions 
    , update =  update 
    , view =  view 
    }
