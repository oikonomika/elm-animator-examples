module Heart exposing (view)

import Element exposing (Element, toRgb)
import Hex
import Svg exposing (..)
import Svg.Attributes exposing (..)


toRgba : Element.Color -> String
toRgba color =
    let
        { red, green, blue } =
            color |> toRgb

        strRed =
            round (red * 255)
                |> Hex.toString
                |> String.padLeft 2 '0'

        strGreen =
            round (green * 255)
                |> Hex.toString
                |> String.padLeft 2 '0'

        strBlue =
            round (blue * 255)
                |> Hex.toString
                |> String.padLeft 2 '0'
    in
    "#" ++ strRed ++ strGreen ++ strBlue


view : List (Element.Attribute msg) -> Element.Color -> Element msg
view attrs color =
    Element.el attrs <|
        Element.html <|
            svg
                [ width "24"
                , height "24"
                , viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ fill <| toRgba color
                    , d "M12 4.419c-2.826-5.695-11.999-4.064-11.999 3.27 0 7.27 9.903 10.938 11.999 15.311 2.096-4.373 12-8.041 12-15.311 0-7.327-9.17-8.972-12-3.27z"
                    ]
                    []
                ]
