module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Nodes as Nodes exposing (Node, Nodes, isSuitableNode)
import App.Visualization exposing (..)
import App.Settings as Settings
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h3, h4)
import Html.Attributes exposing (class, style)
import FormatNumber.Locales exposing (usLocale, Locale, Decimals(..))
import FormatNumber exposing (format)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Svg exposing (Svg, g, a, rect, svg, line, text_)
import Svg.Attributes exposing (alignmentBaseline, xlinkHref, fontSize, fill, height, stroke, strokeWidth, strokeDasharray, textAnchor, transform, width, x, y, x1, x2, y1, y2)
import App.Configuration exposing (Daemons)
import List.Extra exposing (mapAccuml)


type alias Model = 
    {
     configuration : Configuration.Model
    , nodes: Nodes.Model
    , settings: Settings.Model
    , viewportSize: (Int, Int)
    , collapsedSidebar: Bool
    }


type alias ContainerData =
    { name : String
    , color: String
    }


sharesLocale : Locale
sharesLocale =
    { usLocale
        | decimals = Exact 2
    }

hourlyLocale : Locale
hourlyLocale =
    { usLocale
        | decimals = Exact 8
    }

view : Model -> Html msg
view model =
    div [ class "pt-1", class "px-3" ]
        [ Util.viewColumnTitle
            "Results"
        , hr [] []
        , viewResultsForController model
        ]

getSuggestedNodes: Model -> List Node
getSuggestedNodes model =
    let
        controllers = model.configuration.controllers
        containers = model.configuration.containers
        boxes = convertToBoxes controllers containers
        visualization = prepareVisualization boxes 
        share = round <| visualization.width
        memory = round <| visualization.height
        output = case model.settings.optimizeOrder of
            Nodes.RegionsThenBox -> 
                -- let
                --    _ = Debug.log "Test" "If you're reading this, don't forget to fix Results.getSuggestedNodes"
                -- in
                List.map -- TODO: HACK: PLZ FIX?? HOW DO REGION FIRST??
                    (\ region -> 
                        -- TODO: Need to figure out what best node is to ensure we duplicate? How do we choose this?
                        Nodes.findOptimalSuggestions model.nodes region "" share memory )
                        model.nodes.filters.regions
            Nodes.BoxThenRegions -> 
                List.map 
                    (\ region -> 
                        Nodes.findOptimalSuggestions model.nodes region "" share memory )
                        model.nodes.filters.regions

    in
        output

viewResultsForController : Model -> Html msg
viewResultsForController model =
    let
        controllers = model.configuration.controllers
        containers = model.configuration.containers
        boxes = convertToBoxes controllers containers
        visualization = prepareVisualization boxes 
        share = round <| visualization.width / 1024
        memory = round <| visualization.height
        showSuggestions = (Dict.isEmpty model.configuration.containers == False)
        suggestions = getSuggestedNodes model
        visualizations = List.map
                        (\node ->
                            let 
                                topWidth = (toFloat node.vCPU * 1024)
                                topHeight = (toFloat node.memory)
                            in  
                            (SuggestedVisualization node.location topWidth topHeight visualization)
                        ) suggestions

        monthlyCost = List.foldl (+) 0 (List.map (getMonthlyPriceForNode model.nodes.pricingType) suggestions)
        yearlyCost = monthlyCost * 12
    in
    div []
        [ 
          if showSuggestions then 
          div [] 
          [ 
             h3 [] [ text ("Total: $" ++ (format sharesLocale monthlyCost) ++ "/mo")]
            , strong [] [ text ("$" ++ format sharesLocale yearlyCost ++ "/yr")]
            , hr [] []
            , div [] (List.map (viewNodeListing model.nodes.pricingType) suggestions)
            , hr [] []
            , text ("Ideal CPU share: " ++ String.fromInt share)
            , br [] []
            , text ("Ideal memory: " ++ Util.formatMegabytes memory) 
            , hr [] []
            , div [] (List.map viewVisualization visualizations)
        ]
        else
            span [] [ text "No results or suggestions available yet."]
        ]


getMonthlyPriceForNode: Nodes.PreferredPricing -> Node -> Float
getMonthlyPriceForNode preferredPricing node =
    let
        output = getBestPriceForNode preferredPricing node
    in
        output * 24 * 30


getBestPriceForNode: Nodes.PreferredPricing -> Node -> Float
getBestPriceForNode preferredPricing node =
    let
        nodePrices = List.filter (Nodes.pricingLambda preferredPricing) node.prices
        prices = List.map mapPrices node.prices
    in
        List.maximum prices |> Maybe.withDefault 0


mapPrices : Nodes.BoxPricing -> Float
mapPrices price =
    case price of
        Nodes.OnDemand _ value -> value
        Nodes.Reserved _ _ _ value -> value


viewNodeListing : Nodes.PreferredPricing -> Node -> Html msg
viewNodeListing prefPrice node =
    div [ style "margin-top" "10px"] [
        Card.config []
        |> Card.block []
            [ Block.text [] [ h4 [] [ text (node.nodeType ++ ", " ++ (node.vCPU |> String.fromInt) ++ "vCPUs, " ++ (node.memory |> Util.formatMegabytes) ++ " (" ++ node.operatingSystem ++")") ] ] 
            , Block.text [] [ text node.location ]
            , Block.custom <| viewPriceList prefPrice node.prices
            ]
        |> Card.view
    ]


viewPriceList : Nodes.PreferredPricing -> List Nodes.BoxPricing -> Html msg 
viewPriceList prefPrice prices =
    let 
        newPrices = List.filter (Nodes.pricingLambda prefPrice) prices
    in
        ul [class "priceList"] (List.map viewPrice newPrices)


viewPrice : Nodes.BoxPricing -> Html msg
viewPrice price =
    case price of
        Nodes.OnDemand rateCode hourlyCost ->
            li [] [ text <| "$" ++ String.fromFloat hourlyCost ++ "/hr ", span [ class "subtle"] [ text (" " ++ rateCode) ] ]

        Nodes.Reserved rateCode contractLength contractType hourlyCost ->
            case contractType of
                Nodes.AllUpFront -> viewReservedAllUpFront rateCode hourlyCost contractLength
                Nodes.NoUpFront -> viewReservedAllNoUpFront rateCode hourlyCost contractLength


viewReservedAllUpFront: String -> Float -> Nodes.ContractLength -> Html msg
viewReservedAllUpFront rateCode hourlyCost contractLength =
    let 
        hourlyStr = format hourlyLocale hourlyCost
        upfrontCost = hourlyCost * (365 * scalar * 24)
        upfrontStr = format sharesLocale upfrontCost
        scalar = case contractLength of
            Nodes.OneYear -> 1
            Nodes.ThreeYear -> 3
    in
        li [] [ text ("$" ++ hourlyStr ++ "/hr (All upfront)")
              , span [class "subtle"] [ text (" " ++ rateCode )]
              ,  ul [] [
                  li [] [
                      text ("$" ++ upfrontStr ++ " upfront with " ++ String.fromInt scalar ++ " year contract")
                  ]
                ]
        ]

viewReservedAllNoUpFront: String -> Float -> Nodes.ContractLength -> Html msg
viewReservedAllNoUpFront rateCode hourlyCost contractLength = 
    let 
        hourlyStr = format hourlyLocale hourlyCost
        scalar = case contractLength of
            Nodes.OneYear -> 1
            Nodes.ThreeYear -> 3
    in
        li [] [ text ("$" ++ hourlyStr ++ "/hr")
              , span [class "subtle"] [ text (" " ++ rateCode) ]
              ,  ul [] [
                  li [] [
                      text ("With " ++ String.fromInt scalar ++ " year contract")
                  ]
                ]
        ]


convertToBoxes : Configuration.Controllers -> Configuration.Containers -> List Box 
convertToBoxes controllers containers =
    let
        containersList = Dict.toList containers

        initialBoxes = List.concatMap (convertToRepeatedBox controllers) containersList
    in
    initialBoxes


convertToRepeatedBox : Configuration.Controllers -> (Int, Configuration.Container) -> List Box
convertToRepeatedBox controllers (id, container) =
    let
        controller = Dict.get container.controllerId controllers |> Maybe.withDefault (Configuration.Controller "" 0 0 App.Configuration.ByCPUShares 0 0 0)
        cpuShare = (toFloat container.cpuShare)
        memory = (toFloat container.memory)
        sortValue =
            case controller.packingStrategy of
                App.Configuration.ByCPUShares -> cpuShare
                App.Configuration.ByMemory -> memory
    in
    List.repeat controller.nominalPods (Box id container.name controller.name container.color 0 0 cpuShare memory sortValue)
