module App.Pod exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Constants exposing (allRegions)
import App.Util as Util exposing (viewFormRowSlider)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Multiselect
import Tuple exposing (first, second)


type alias Model =
    Configuration.Model


type Msg
    = UpdateMinPods Int String
    | UpdateMaxPods Int String
    | UpdateNominalPods Int String



-- https://elmseeds.thaterikperson.com/elm-multiselect


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMinPods id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | minPods = Util.toInt value })) model.services }

        UpdateMaxPods id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | maxPods = Util.toInt value })) model.services }

        UpdateNominalPods id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | nominalPods = Util.toInt value })) model.services }


view : Int -> Configuration.Service -> Configuration.Nodes -> Html Msg
view id service nodes =
    div []
        [ Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " - Pod Settings") ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormRowSlider "Min. Pods" ((String.fromInt <| service.minPods) ++ " Pods") service.minPods 1 service.maxPods 1 (UpdateMinPods id)
                        , Util.viewFormRowSlider "Nom. Pods" ((String.fromInt <| service.nominalPods) ++ " Pods") service.nominalPods service.minPods service.maxPods 1 (UpdateNominalPods id)
                        , Util.viewFormRowSlider "Max. Pods" ((String.fromInt <| service.maxPods) ++ " Pods") service.maxPods service.minPods 100 1 (UpdateMaxPods id)
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text "Nodes Overview" ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormLabel "Total Memory" "Total memory of all nodes in this service combined." ((String.fromFloat <| sumMemory nodes * toFloat service.nominalPods) ++ " GiB")
                        , Util.viewFormLabel "Total CPU Shares" "CPU Shares required for all nodes in one pod" ((String.fromInt <| sumCPUShare nodes * service.nominalPods) ++ "/1024")
                        , Util.viewFormLabel "Total Bandwidth" "Bandwidth required for all nodes in one pod" ((String.fromInt <| sumBandwidth nodes * service.nominalPods) ++ " GiB/sec")
                        , Util.viewFormLabel "IO Total" "IO requirements for all nodes in one pod" (sumIoops service nodes)
                        ]
                ]
            |> Card.view
        ]


sumMemory : Configuration.Nodes -> Float
sumMemory nodes =
    List.sum (List.map (\node -> toFloat node.memory) (Dict.values nodes)) / 1000


sumCPUShare : Configuration.Nodes -> Int
sumCPUShare nodes =
    List.sum (List.map (\node -> node.cpuShare) (Dict.values nodes))


sumBandwidth : Configuration.Nodes -> Int
sumBandwidth nodes =
    List.sum (List.map (\node -> node.bandwidth) (Dict.values nodes))


sumIoops : Configuration.Service -> Configuration.Nodes -> String
sumIoops service nodes =
    let
        -- This feels like a lot of duplicated code
        nodesWithEBS =
            List.filter (\node -> node.useEBS == True) (Dict.values nodes)

        nodesWoEBS =
            List.filter (\node -> node.useEBS == False) (Dict.values nodes)

        otherSum =
            List.sum (List.map (\node -> node.ioops) (Dict.values nodes))

        someUseEBS =
            List.length nodesWithEBS > 0
    in
    if someUseEBS then
        String.fromInt (List.length nodesWithEBS) ++ " node(s) using EBS. Total: " ++ String.fromInt otherSum ++ "MiB/sec"

    else
        String.fromInt (otherSum * service.nominalPods) ++ " MiB/sec"
