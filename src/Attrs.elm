module Attrs exposing
    ( centeredBy
    , disabledTextSelection
    , hsl
    )

import Element exposing (Attr, htmlAttribute)
import Html.Attributes as RawAttrs


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
