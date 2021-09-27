module App.Settings exposing (Model, Msg(..), init, subscriptions, update, view)
import App.Nodes as Nodes exposing (FilterType, Model, Filters, update, Msg(..), PreferredPricing(..), OptomizationOrder(..))
import App.Constants exposing (nodeTypes, allRegions)
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect
import App.Nodes exposing (Nodes)

type alias Model =
    { excludedNodes : Multiselect.Model
    , excludedSystems: Multiselect.Model
    , includedRegions: Multiselect.Model
    , preferredPricing: PreferredPricing
    , optimizeOrder: OptomizationOrder
    }



init : Model
init =
    { excludedNodes = Multiselect.initModel (List.map (\nodeType -> (nodeType, String.toUpper nodeType)) nodeTypes) "A"
    , excludedSystems = Multiselect.initModel [("SUSE", "SUSE"), ("Windows", "Windows"), ("Linux", "Linux"), ("RHEL", "RHEL")] "B"
    , includedRegions = Multiselect.initModel (List.map (\region -> (region, region)) allRegions) "C"
    , preferredPricing = Reserved1Yr
    , optimizeOrder = BoxThenRegions
    }



-- There's a better way to do this...


type Msg
    = UpdateExcludedNodes Multiselect.Msg
    | UpdateExcludedOS Multiselect.Msg
    | UpdateIncludedRegions Multiselect.Msg
    | SetPricingPreference PreferredPricing
    | SetOptOrder OptomizationOrder

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExcludedNodes nodesChangedMessage ->
            let
                ( newExcludedNodes, subCmd, _ ) =
                    Multiselect.update nodesChangedMessage model.excludedNodes
            in
            ( { model | excludedNodes = newExcludedNodes }, Cmd.map UpdateExcludedNodes subCmd )

        UpdateExcludedOS osChangedMessage ->
            let
                ( newExcludedos, subCmd, _ ) =
                    Multiselect.update osChangedMessage model.excludedSystems
            in
            ( { model | excludedSystems = newExcludedos}, Cmd.map UpdateExcludedOS subCmd )

        UpdateIncludedRegions regionsChangedMessage ->
            let
                ( newExcludedRegions, subCmd, _) =
                    Multiselect.update regionsChangedMessage model.includedRegions
            in
            ( { model | includedRegions = newExcludedRegions}, Cmd.map UpdateIncludedRegions subCmd )

        SetPricingPreference pref -> 
            ({model | preferredPricing = pref}, Cmd.none)

        SetOptOrder order ->
            ({model | optimizeOrder = order}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Sub.map UpdateExcludedNodes <| Multiselect.subscriptions model.excludedNodes
    , Sub.map UpdateExcludedOS <| Multiselect.subscriptions model.excludedSystems
    , Sub.map UpdateIncludedRegions <| Multiselect.subscriptions model.includedRegions
    ]
    


view : Model -> Html Msg
view model =
    Card.config []
        |> Card.header [] [ text "Filters" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Node Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedNodes <| Multiselect.view model.excludedNodes
                            , Form.help [] [ text "Exclude specific ECS nodes. These will be ignored during the cluster optimization calculation." ]
                            ]
                        ]
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Operating System Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedOS <| Multiselect.view model.excludedSystems
                            , Form.help [] [ text "Exclude specific operating systems from the EC2 nodes." ]
                            ]
                        ]
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Included Regions" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateIncludedRegions <| Multiselect.view model.includedRegions
                            , Form.help [] [ text "For each of these regions, we will try and find an optimal node type." ]
                            ]
                        ]
                    , hr [] []
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Pricing Preference" ]
                        , Form.col [ Col.sm9 ]
                            [ Fieldset.config
                                |> Fieldset.asGroup
                                |> Fieldset.children
                                    (Radio.radioList "pricingOptions"
                                        [ Radio.create [ Radio.id "reserved_1yr", Radio.checked (model.preferredPricing == Reserved1Yr), Radio.onClick (SetPricingPreference Reserved1Yr) ] "Reserved (1 year)"
                                        , Radio.create [ Radio.id "reserved_3yr", Radio.checked (model.preferredPricing == Reserved3Yr), Radio.onClick (SetPricingPreference Reserved3Yr) ] "Reserved (3 year)"
                                        , Radio.create [ Radio.id "ondemand", Radio.checked (model.preferredPricing == OnDemandPricing), Radio.onClick (SetPricingPreference OnDemandPricing) ] "On-Demand"
                                        ]
                                    )
                                |> Fieldset.view
                            ]
                        ]
                        , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Order of Optimization" ]
                        , Form.col [ Col.sm9 ]
                            [ Fieldset.config
                                |> Fieldset.asGroup
                                |> Fieldset.children
                                    (Radio.radioList "orderOfOptimizations"
                                        [ Radio.create [ Radio.id "reg-then-box", Radio.checked (model.optimizeOrder == RegionsThenBox), Radio.onClick (SetOptOrder RegionsThenBox) ] "Optimize by Regions → CPU/Mem"
                                        , Radio.create [ Radio.id "box-then-reg", Radio.checked (model.optimizeOrder == BoxThenRegions), Radio.onClick (SetOptOrder BoxThenRegions) ] "Optimize by CPU/Mem → Regions"
                                        ]
                                    )
                                |> Fieldset.view
                            ]
                        ]
                    ]
            ]
        |> Card.view
