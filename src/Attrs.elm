module Attrs exposing (alpha, color, xy)

import Animator exposing (Movement, Timeline)
import Animator.Inline
import Color as RawColor
import Element exposing (Attr, Color)


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
