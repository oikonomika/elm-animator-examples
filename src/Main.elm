module Main exposing (..)

import Animator exposing (Animator, Timeline)
import Attrs
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Heart
import Html exposing (Html)
import Html.Attributes
import Random
import Time


white : Color
white =
    rgb255 255 255 255


blackAlpha : Color
blackAlpha =
    rgba255 0 0 0 0.1


blackAlpha2 : Color
blackAlpha2 =
    rgba255 0 0 0 0.2


aliceBlue : Color
aliceBlue =
    rgb255 236 249 255


deepPink : Color
deepPink =
    rgb255 255 116 177


neonPink : Color
neonPink =
    rgb255 255 161 207


slate : Color
slate =
    rgb255 80 86 90


type Msg
    = Tick Time.Posix
    | Check Bool
    | MouseDown
    | MouseUp
    | GotParticles (List Particle)


type PopStep
    = PopOrigin
    | PopShrinking


type ParticlesStep
    = ParticlesOrigin
    | ParticlesEmmiting
    | ParticlesAbsorb


type alias Particle =
    { angle : Float
    , distance : Float
    , size : Float
    , hue : Float
    }


randomParticle : Random.Generator Particle
randomParticle =
    Random.map4 Particle
        (Random.float 0 360.0)
        (Random.float 50.0 100.0)
        (Random.float 8.0 16.0)
        (Random.float 200.0 360.0)


randomParticleList : Int -> Random.Generator (List Particle)
randomParticleList n =
    randomParticle
        |> Random.list n


type alias Model =
    { checked : Timeline Bool
    , pressed : Timeline Bool
    , heartStep : Timeline PopStep
    , particlesStep : Timeline ParticlesStep
    , particles : List Particle
    }


initialModel : Model
initialModel =
    { checked = Animator.init False
    , pressed = Animator.init False
    , heartStep = Animator.init PopOrigin
    , particlesStep = Animator.init ParticlesOrigin
    , particles = []
    }


animator : Animator Model
animator =
    Animator.animator
        |> Animator.watching .checked (\checked model -> { model | checked = checked })
        |> Animator.watching .pressed (\pressed model -> { model | pressed = pressed })
        |> Animator.watching .heartStep (\heartStep model -> { model | heartStep = heartStep })
        |> Animator.watching .particlesStep (\particlesStep model -> { model | particlesStep = particlesStep })


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


playParticles : List Particle -> Model -> Model
playParticles particles model =
    let
        checked =
            model.checked |> Animator.current

        popHeart : Timeline PopStep -> Timeline PopStep
        popHeart =
            Animator.interrupt
                [ Animator.event Animator.immediately PopOrigin
                , Animator.event Animator.veryQuickly PopShrinking
                , Animator.event Animator.verySlowly PopOrigin
                ]

        emitParticles : Timeline ParticlesStep -> Timeline ParticlesStep
        emitParticles =
            Animator.interrupt
                [ Animator.event Animator.immediately ParticlesOrigin
                , Animator.event Animator.slowly ParticlesEmmiting
                , Animator.event Animator.immediately ParticlesAbsorb
                ]
    in
    if not checked then
        { model
            | heartStep = popHeart model.heartStep
            , particlesStep = emitParticles model.particlesStep
            , particles = particles
        }

    else
        model


playCheck : Bool -> Model -> Model
playCheck checked model =
    if checked then
        { model | checked = Animator.go Animator.slowly checked model.checked }

    else
        { model | checked = Animator.go Animator.immediately checked model.checked }


playPress : Bool -> Model -> Model
playPress pressed model =
    { model | pressed = Animator.go Animator.quickly pressed model.pressed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( Animator.update posix animator model, Cmd.none )

        Check checked ->
            ( model
                |> playCheck checked
            , Cmd.none
            )

        MouseDown ->
            ( model
                |> playPress True
            , Cmd.none
            )

        MouseUp ->
            ( model
                |> playPress False
            , randomParticleList 30 |> Random.generate GotParticles
            )

        GotParticles particles ->
            ( model
                |> playParticles particles
            , Cmd.none
            )


viewParticle : Timeline ParticlesStep -> Particle -> Element msg
viewParticle particlesStep particle =
    el
        ([ Attrs.xy particlesStep
            (\state ->
                case state of
                    ParticlesOrigin ->
                        { x = Animator.at 0
                        , y = Animator.at 0
                        }

                    ParticlesEmmiting ->
                        { x = Animator.arriveSmoothly 0 <| Animator.at <| sin (degrees particle.angle) * particle.distance
                        , y = Animator.arriveSmoothly 0 <| Animator.at <| cos (degrees particle.angle) * particle.distance
                        }

                    ParticlesAbsorb ->
                        { x = Animator.at <| 0
                        , y = Animator.at <| 0
                        }
            )
         , Attrs.alpha particlesStep <|
            \state ->
                case state of
                    ParticlesOrigin ->
                        Animator.at 0

                    ParticlesEmmiting ->
                        Animator.at 0.3

                    ParticlesAbsorb ->
                        Animator.at 0
         , Attrs.singleton <| Border.rounded <| round (particle.size / 2)
         , Attrs.singleton <| width <| px <| round particle.size
         , Attrs.singleton <| height <| px <| round particle.size
         , Attrs.singleton <| htmlAttribute <| Html.Attributes.style "backgroundColor" ("hsl(" ++ String.fromFloat particle.hue ++ ", 60%, 60%)")
         , Attrs.singleton <| htmlAttribute <| Html.Attributes.style "position" "absolute"
         , Attrs.singleton <| htmlAttribute <| Html.Attributes.style "left" ("calc(50% + " ++ String.fromFloat particle.size ++ "px)")
         , Attrs.singleton <| htmlAttribute <| Html.Attributes.style "top" ("calc(50% + " ++ String.fromFloat particle.size ++ "px)")
         ]
            |> Attrs.batch
        )
        none


viewParticles : Timeline ParticlesStep -> List Particle -> Element msg
viewParticles particlesStep particles =
    particles
        |> List.map (viewParticle particlesStep)
        |> row []


viewHeart : Model -> Element Msg
viewHeart model =
    el
        [ centerX
        , behindContent <| viewParticles model.particlesStep model.particles
        , scale <|
            Animator.move model.heartStep <|
                \state ->
                    case state of
                        PopOrigin ->
                            Animator.at 1.0 |> Animator.arriveSmoothly 1.0

                        PopShrinking ->
                            Animator.at 0.6
        ]
    <|
        Heart.view [] <|
            Attrs.color model.checked <|
                \state ->
                    if state then
                        neonPink

                    else
                        white


viewLabel : Element Msg
viewLabel =
    el
        [ centerX
        , Font.color white
        , Font.center
        , Font.semiBold
        , Font.size 24
        , htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
    <|
        text "LIKE"


viewButton : Model -> Element Msg
viewButton model =
    Input.checkbox
        [ Border.rounded 16
        , Border.color white
        , Background.color deepPink
        , Events.onMouseDown MouseDown
        , Events.onMouseUp MouseUp
        , Border.glow
            (Attrs.color model.pressed <|
                \state ->
                    if state then
                        blackAlpha2

                    else
                        blackAlpha
            )
            16
        , scale <|
            Animator.move model.pressed <|
                \state ->
                    if state then
                        Animator.at 0.9 |> Animator.arriveSmoothly 1.0

                    else
                        Animator.at 1.0 |> Animator.arriveSmoothly 1.0
        ]
        { onChange = Check
        , icon =
            always <|
                row
                    [ width <| px 128
                    , height <| px 64
                    , spacing 16
                    ]
                    [ viewHeart model
                    , viewLabel
                    ]
        , checked = model.checked |> Animator.current
        , label = Input.labelHidden "Like"
        }


view : Model -> Html Msg
view model =
    Element.layout [ Background.color aliceBlue ] <|
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
