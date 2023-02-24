module Main exposing (..)

import Animator exposing (Animator, Timeline)
import Browser
import Element exposing (..)
import Element.Input as Input
import Element.Border as Border
import Element.Background as Background
import Html exposing (Html)
import Time


white: Color
white = rgb255 255 255 255

slate: Color
slate = rgb255 80 86 90


type Msg
    = Tick Time.Posix
    | Check Bool


type alias Model =
    { checked : Timeline Bool
    }


initialModel : Model
initialModel =
    { checked = Animator.init False
    }


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watching .checked (\checked model -> { model | checked = checked })


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( Animator.update posix animator model, Cmd.none )

        Check checked ->
            ( { model
                | checked = Animator.go Animator.slowly checked model.checked
              }
            , Cmd.none
            )


viewButton : Model -> Element Msg
viewButton model =
    Input.checkbox
        [ Border.width 1
        , width <| px (32 * 3 + 8 * 2)
        , padding 8
        , Background.color slate
        ]
        { onChange = Check
        , icon = \_ ->
            Element.el
                [ width <| px 32
                , height <| px 32
                , Background.color white
                , moveRight
                    <| Animator.move model.checked <| \state ->
                        if state then
                            Animator.at (32 * 2)
                        else
                            Animator.at 0
                ]
                Element.none
        , checked = model.checked |> Animator.current
        , label = Input.labelHidden"check me"
        }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ centerX, centerY ]
            [ viewButton model ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
