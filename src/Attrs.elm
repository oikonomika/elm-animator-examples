module Attrs exposing (alpha, batch, color, singleton, xy)

import Animator exposing (Movement, Timeline)
import Animator.Inline
import Color as RawColor
import Element exposing (Attr, Color)


color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Animator.color timeline (lookup >> Element.toRgb >> RawColor.fromRgba)
        |> (RawColor.toRgba >> Element.fromRgb)


batch : List (List (Attr decorative msg)) -> List (Attr decorative msg)
batch attrs =
    List.concat attrs


singleton : Attr decorative msg -> List (Attr decorative msg)
singleton attr =
    List.singleton attr


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
