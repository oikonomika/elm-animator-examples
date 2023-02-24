module AnimatorWrapper exposing (batch, color, singleton, xy)

import Animator exposing (Movement, Timeline)
import Color
import Element


color : Timeline state -> (state -> Element.Color) -> Element.Color
color timeline lookup =
    Animator.color timeline (lookup >> Element.toRgb >> Color.fromRgba)
        |> (Color.toRgba >> Element.fromRgb)


batch : List (List (Element.Attr decorative msg)) -> List (Element.Attr decorative msg)
batch attrs =
    List.concat attrs


singleton : Element.Attr decorative msg -> List (Element.Attr decorative msg)
singleton attr =
    List.singleton attr


xy : Timeline state -> (state -> { x : Movement, y : Movement }) -> List (Element.Attr decorative msg)
xy timeline lookup =
    let
        { x, y } =
            Animator.xy timeline lookup
    in
    [ Element.moveRight x
    , Element.moveDown y
    ]
