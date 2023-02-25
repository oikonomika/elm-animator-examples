module Attrs exposing (alpha, centeredBy, color, disabledTextSelection, hsl, xy)

import Animator exposing (Movement, Timeline)
import Animator.Inline
import Color as RawColor
import Element exposing (Attr, Color, htmlAttribute)
import Html.Attributes as RawAttrs


color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Animator.color timeline (lookup >> Element.toRgb >> RawColor.fromRgba)
        |> (RawColor.toRgba >> Element.fromRgb)


xy : Timeline state -> (state -> { x : Movement, y : Movement }) -> List (Attr decorative msg)
xy timeline lookup =
    let
        { x, y } =
            Animator.xy timeline lookup
    in
    [ Element.moveRight x
    , Element.moveDown y
    ]


alpha : Timeline state -> (state -> Movement) -> List (Attr () msg)
alpha timeline lookup =
    [ Element.htmlAttribute <|
        Animator.Inline.opacity timeline lookup
    ]


hsl : { hue : Float, saturation : Float, lightness : Float } -> Attr () msg
hsl { hue, saturation, lightness } =
    let
        strHue =
            String.fromFloat hue

        strSaturation =
            (String.fromFloat <| saturation * 100.0) ++ "%"

        strLightness =
            (String.fromFloat <| lightness * 100.0) ++ "%"

        hslValue =
            "hsl(" ++ strHue ++ ", " ++ strSaturation ++ ", " ++ strLightness ++ ")"
    in
    htmlAttribute <| RawAttrs.style "backgroundColor" hslValue


centeredBy : Float -> Float -> List (Attr () msg)
centeredBy width height =
    [ htmlAttribute <| RawAttrs.style "position" "absolute"
    , htmlAttribute <| RawAttrs.style "left" ("calc(50% + " ++ String.fromFloat width ++ "px)")
    , htmlAttribute <| RawAttrs.style "top" ("calc(50% + " ++ String.fromFloat height ++ "px)")
    ]


disabledTextSelection : Attr () msg
disabledTextSelection =
    htmlAttribute <| RawAttrs.style "user-select" "none"
