module App.Controller exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration exposing (PackingStrategy)
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)


type alias Model =
    Configuration.Model


type Msg
    = UpdateScalingTarget Int String
    | UpdatePackingStrategy Int PackingStrategy


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScalingTarget id value ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | scalingTarget = Util.toInt value })) model.controllers }

        UpdatePackingStrategy id strategy ->
            { model | controllers = Dict.update id (Maybe.map (\controller -> { controller | packingStrategy = strategy })) model.controllers }


view : Int -> Configuration.Controller -> Html Msg
view id controller =
    Card.config []
        |> Card.header [] [ text controller.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Util.viewFormRowSlider "Scaling Target" ((String.fromInt <| controller.scalingTarget) ++ "%") controller.scalingTarget 0 100 1 (UpdateScalingTarget id)
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Packing Strategy" ]
                        , Form.col [ Col.sm9 ]
                            [ Fieldset.config
                                |> Fieldset.asGroup
                                |> Fieldset.children
                                    (Radio.radioList "packingStrategy"
                                        [ Radio.create [ Radio.id "cpushares", Radio.checked (controller.packingStrategy == Configuration.ByCPUShares), Radio.onClick (UpdatePackingStrategy id Configuration.ByCPUShares) ] "By CPU Shares"
                                        , Radio.create [ Radio.id "memory", Radio.checked (controller.packingStrategy == Configuration.ByMemory), Radio.onClick (UpdatePackingStrategy id Configuration.ByMemory) ] "By Memory"
                                        ]
                                    )
                                |> Fieldset.view
                            ]
                        ]
                    ]
            ]
        |> Card.view
