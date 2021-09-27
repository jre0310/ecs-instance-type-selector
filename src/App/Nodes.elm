port module App.Nodes exposing (..)

import App.ApiDecoders as ApiDecoders
import Json.Decode exposing (Error(..), decodeString)
import Array
import Bootstrap.Utilities.Border exposing (rounded)
import Maybe.Extra exposing (..)
import List.Extra exposing (..)
import Html exposing (i)

---- PORTS ----

port requestNodes : ( String, String, Int ) -> Cmd msg

port receiveNodes : (String -> msg) -> Sub msg


-- Model

type PreferredPricing
    = Reserved1Yr
    | Reserved3Yr
    | OnDemandPricing

type OptomizationOrder
    = RegionsThenBox
    | BoxThenRegions

type alias Model =
     { nodes: Nodes
     , filters: Filters
     , pricingType: PreferredPricing
     }
    
type alias Filters = 
    { os: List String
    , nodeType: List String
    , regions: List String
    }

type FilterType
    = OS
    | NodeType
    | Region

type Msg
    = LoadNodes (Result Json.Decode.Error ApiDecoders.ProductsResponse)
    | SetFilters FilterType (List String)
    | SetPreferredPricing PreferredPricing


type alias Nodes = List Node
type alias Node = 
     { sku: String                 -- The SKU (ID) of the EC2 node
     , nodeType: String        -- The node type (e.g. "m5ad.12xlarge")
     , location: String            -- This relates to the region, but for now, probably a good idea to store this (e.g. "EU (Ireland)")
     , operatingSystem: String     -- Probably a good idea to have this for future purposes
     , memory: Int                 -- The memory available, in MB. Make sure we convert to MB from whatever the API gives us.
     , vCPU: Int                   -- Number of vCPUs that this node has available
     , prices: List BoxPricing
     }

type ContractLength
    = OneYear
    | ThreeYear

type ContractType
    = AllUpFront
    | NoUpFront
--    | PartialUpFront Float -- NOTE: Not dealing with this. Ignoring for l8r.

type BoxPricing
    = OnDemand String Float -- OnDemand {HourlyCost}
    | Reserved String ContractLength ContractType Float



defaultRegion : String 
defaultRegion =
    "us-east-1"

-- Setup

init : Model
init = {nodes=[], filters={os=[], nodeType=[], regions=[]}, pricingType = Reserved1Yr}


defaultNode : Node
defaultNode =
    { sku = ""
      , nodeType = ""
      , location = ""
      , operatingSystem = ""
      , memory = 0
      , vCPU = 0
      , prices = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNodes (LoadNodes << decodeString ApiDecoders.productsResponseDecoder)


numNodesBatched : Int
numNodesBatched =
    100


maxNodesTesting : Int 
maxNodesTesting =
    1500


--updateWithFilters : 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadNodes (Ok response) ->
            let
                simplified = mapToNodes response.priceList
                totalCount = List.length model.nodes

                region = defaultRegion
                nextCommand =
                -- Ensure we do not exceed our max limit of nodes
                    if totalCount < maxNodesTesting - numNodesBatched then
                        requestNodes ( region, response.nextToken, numNodesBatched )
                    else
                        Cmd.none
            in
            ( {model | nodes = model.nodes ++ simplified}, nextCommand )

        LoadNodes (Err err) ->
            -- let
            --    _ = Debug.log "Node Load Error" err
            -- in
            ( model, Cmd.none )

        SetFilters filterType filterData ->
            case filterType of
                OS -> 
                    let
                        filters = model.filters
                    in
                    ({model | filters = { filters | os = filterData}}, Cmd.none)
                NodeType ->
                    let
                        filters = model.filters
                    in
                    ({model | filters = { filters | nodeType = filterData}}, Cmd.none)
                Region ->
                    let
                        filters = model.filters
                    in
                    ({model | filters = { filters | regions = filterData}}, Cmd.none)
        SetPreferredPricing ptype -> 
            ({model | pricingType = ptype}, Cmd.none)
                

-- Mapping

mapToNodes : List ApiDecoders.PriceListing -> Nodes
mapToNodes original =
    values <| List.map priceListingToNode original 


findOptimalSuggestions: Model -> String -> String -> Int -> Int -> Node 
findOptimalSuggestions model region nodeType vcpu memory =
   let 
        suggestions = model.nodes 
            |> List.filter (isSuitableNode vcpu memory)
            |> List.filter (isNotExludedNode model.filters)
            |> List.filter (filterByPricing model.pricingType)
            |> List.filter (filterByRegion region)
            |> List.filter (filterByNodeType nodeType)
            |> List.sortBy .memory
            |> List.sortBy .vCPU
            --|> List.sortBy lowestPrice
             -- TODO: sort by lowest price
   in
        List.head suggestions |> Maybe.withDefault defaultNode


filterByPricing: PreferredPricing -> Node -> Bool
filterByPricing preferred node =
    List.any (pricingLambda preferred) node.prices


filterByRegion: String -> Node -> Bool
filterByRegion region node =
    String.startsWith region node.location 


filterByNodeType: String -> Node -> Bool
filterByNodeType nodeType node =
    String.startsWith nodeType node.nodeType 


pricingLambda: PreferredPricing -> BoxPricing -> Bool
pricingLambda preferred pricing =
    case preferred of
        Reserved1Yr -> 
            case pricing of
                Reserved _ length _ _ -> 
                    case length of
                        OneYear -> True
                        _ -> False
                _ -> False
        Reserved3Yr ->
            case pricing of
                Reserved _ length _ _ -> 
                    case length of
                        ThreeYear -> True
                        _ -> False
                _ -> False
        OnDemandPricing ->
            case pricing of
                OnDemand _ _ -> True
                _ -> False    

--lowestPrice: Node -> Node -> Nodes -> Nodes
--lowestPrice node compare nodes =
 --   []



isNotExludedNode: Filters -> Node -> Bool 
isNotExludedNode filters node =
    let
        osExcluded = List.member node.operatingSystem filters.os
        typeExcluded = isNotExcludedNodeType filters node -- TODO: Fix and actually make this work

        regionIncluded = isIncludedRegion filters node
    in
        not osExcluded && not typeExcluded && regionIncluded

isNotExcludedNodeType: Filters -> Node -> Bool 
isNotExcludedNodeType filters node =
    let
        itype = node.nodeType
    in
        List.any (\item -> String.startsWith item itype) filters.nodeType


isIncludedRegion: Filters -> Node -> Bool 
isIncludedRegion filters node =
    let
        region = node.location
    in
        List.any (\item -> String.startsWith item region) filters.regions
    
isSuitableNode : Int -> Int -> Node -> Bool
isSuitableNode vcpu memory node =
    let
        share = round <| toFloat vcpu
    in
    node.memory >= memory && (node.vCPU * 1024) >= share


priceListingToNode : ApiDecoders.PriceListing -> Maybe Node
priceListingToNode original =
    let
        product = original.product
        attributes = product.attributes
        sku = product.sku
        nodeType = attributes.nodeType
        location = attributes.location
        operatingSystem = attributes.operatingSystem
        memory = attributes.memory |> convertMemoryStringToMiB
        vCPU = attributes.vCPU |> String.toInt |> Maybe.withDefault 0
        onDemandPrices = List.concatMap termToPrices original.terms.onDemand |> filterZeroValues
        reservedPrices = List.concatMap termToPrices original.terms.reserved |> filterZeroValues

        pricingList = onDemandPrices ++ reservedPrices
    in 
        if memory > 0 && vCPU >= 0 then
            Just (Node sku nodeType location operatingSystem memory vCPU pricingList)
        else
            Nothing


filterZeroValues: List BoxPricing -> List BoxPricing
filterZeroValues prices =
    let
        keepPrice: BoxPricing -> Bool
        keepPrice price =
           case price of
                OnDemand _ value -> (value > 0)
                Reserved _ _ _ value -> (value > 0)

    in
        List.filter keepPrice prices


termToPrices : ApiDecoders.Term -> List BoxPricing
termToPrices term =
    let
        termAttributes = term.termAttributes
        priceDimensions = term.priceDimensions
    in
        List.map (termToBoxPricing termAttributes) priceDimensions


termToBoxPricing : ApiDecoders.TermAttributes -> ApiDecoders.PriceDimension -> BoxPricing
termToBoxPricing termAttributes dimension =
    let 
        unitPrice = String.toFloat dimension.pricePerUnit.usd |> Maybe.withDefault 0
        rateCode = dimension.rateCode
    in
        -- Ideally, these should be Maybe's, not just be empty strings
        if String.isEmpty termAttributes.leaseContractLength ||
           String.isEmpty termAttributes.purchaseOption then
            OnDemand rateCode unitPrice
        else
            let
                contractLength = contractLengthStringConverter termAttributes.leaseContractLength
                purchaseOption = contractTypeStringConverter termAttributes.purchaseOption
                finalLength = Maybe.withDefault OneYear contractLength
                yearScalar = case finalLength of
                    OneYear -> 1
                    ThreeYear -> 3
            in
            Reserved rateCode finalLength (Maybe.withDefault NoUpFront purchaseOption) ((unitPrice / (365 * yearScalar)) / 24)


contractLengthStringConverter: String -> Maybe ContractLength
contractLengthStringConverter value =
    case value of
        "1yr" -> Just OneYear
        "3yr" -> Just ThreeYear
        _ -> Nothing

contractTypeStringConverter: String -> Maybe ContractType
contractTypeStringConverter value =
    case value of 
        "All Upfront" -> Just AllUpFront
        "No Upfront" -> Just NoUpFront
        _ -> Nothing

convertMemoryStringToMiB : String -> Int
convertMemoryStringToMiB input =
    let
        result = String.split " " input |> Array.fromList
        value = Array.get 0 result |> Maybe.withDefault "" |> String.toInt |> Maybe.withDefault 0
        unit = Array.get 1 result |> Maybe.withDefault ""
    in
        case unit of
           "MiB" -> value
           "GiB" -> value * 1024
           "TiB" -> value * 1024 * 1024
           _ -> 0
