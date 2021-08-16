module App.Node exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Daemon as Daemon
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    Configuration.Model


type Msg
    = UpdateCPUShare Int String
    | UpdateMemory Int String
    | UpdateIoops Int String
    | UpdateEBS Int Bool
    | UpdateBandwidth Int String
    | ToggleMoreMemory Int Bool
    | DaemonMsg Daemon.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        DaemonMsg daemonMsg ->
            Daemon.update daemonMsg model

        UpdateCPUShare id value ->
            { model | nodes = Dict.update id (Maybe.map (\node -> { node | cpuShare = Util.toInt value })) model.nodes }

        UpdateMemory id value ->
            { model | nodes = Dict.update id (Maybe.map (\node -> { node | memory = Util.toInt value })) model.nodes }

        UpdateIoops id value ->
            { model | nodes = Dict.update id (Maybe.map (\node -> { node | ioops = Util.toInt value })) model.nodes }

        UpdateEBS id checked ->
            { model | nodes = Dict.update id (Maybe.map (\node -> { node | useEBS = checked })) model.nodes }

        UpdateBandwidth id value ->
            { model | nodes = Dict.update id (Maybe.map (\node -> { node | bandwidth = Util.toInt value })) model.nodes }

        ToggleMoreMemory id checked ->
            { model
                | nodes =
                    Dict.update id
                        (Maybe.map
                            (\node ->
                                { node
                                    | showExtraMemory = checked
                                    , memory =
                                        if checked then
                                            node.memory

                                        else
                                            32000
                                }
                            )
                        )
                        model.nodes
            }


determineMaxNodeMemory : Bool -> Int
determineMaxNodeMemory useMoreMem =
    if useMoreMem then
        24576000

    else
        32000


determineNodeMemStep : Bool -> Int
determineNodeMemStep extraMemEnabled =
    if extraMemEnabled then
        1000

    else
        250



-- this function feels odd



view : Int -> Configuration.Node -> Configuration.Daemons -> Html Msg
view id node daemons =
    Card.config []
        |> Card.header [] [ text node.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    -- these Util calls are a bit odd, but do make the code a bit more organized.
                    [ Util.viewFormRowSlider "CPU Share" ((String.fromInt <| node.cpuShare) ++ "/1024 CPU Share") node.cpuShare 8 1024 8 (UpdateCPUShare id)
                    , hr [] []
                    , Util.showIf (node.memory >= 32000 || node.showExtraMemory) (Util.viewFormCheckbox "Show more memory options" "" node.showExtraMemory (ToggleMoreMemory id))
                    , Util.viewFormRowSlider "Memory" (Util.formatMegabytes node.memory) node.memory 250 (determineMaxNodeMemory node.showExtraMemory) (determineNodeMemStep node.showExtraMemory) (UpdateMemory id)
                    , hr [] []
                    , Util.viewFormCheckbox "Use Elastic Block Storage" "" node.useEBS (UpdateEBS id)
                    , Util.viewFormRowSlider "IOOPs" ((String.fromInt <| node.ioops) ++ " MiB/sec") node.ioops 4750 19000 1000 (UpdateIoops id)
                    , hr [] []
                    , Util.viewFormRowSlider "Bandwidth" ((String.fromInt <| node.bandwidth) ++ " GiB/sec") node.bandwidth 1 25 1 (UpdateBandwidth id)
                    , hr [] []
                    , span [] [text (String.fromInt (Tuple.first (Daemon.sumDaemonResources daemons id)) ++ " Total Daemon CPU Shares")]
                    , br [] []
                    , span [] [text (String.fromInt (Tuple.second (Daemon.sumDaemonResources daemons id)) ++ "Mb Total Daemon Memory")]
                    , hr [] []
                    , Html.map DaemonMsg (Daemon.view daemons id node)
                    ]
            ]

        |> Card.view
        
