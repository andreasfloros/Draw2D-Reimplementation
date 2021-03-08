﻿module BusWire

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Demo------------------------------//
//------------------------------------------------------------------------//

// The Segment and Dir types are used extensively here, see Helpers module for details on these
// The below code is flexible in that it only uses interfaces to access port and symbol info
// The necessary interface functions are implemented in Symbol and CommonTypes modules

// splits the stretched port extension of a segment into the excess part, a 0 length segment and the port extension
// should only be used when the excess length is strictly greater than 0
let splitSegmentsAtEndSeg isStart (segments: Segment list)=
    let segmentIndex = if isStart then 0 else segments.Length - 1
    let segmentOfInterest = segments.[segmentIndex]
    let newPortSeg = // dirOfSeg is safe here
        (if segmentIndex = 0 then (segmentOfInterest.Start, dirOfSeg segmentOfInterest, true)
         else (segmentOfInterest.End, getOppositeDir (dirOfSeg segmentOfInterest), false))
        |||> CommonTypes.getPortExtension 
    let modifiedSegments = segments
                           |> List.mapi (fun idx segment -> 
                                                if idx = segmentIndex then 
                                                    if segmentIndex = 0 then segOf newPortSeg.End segmentOfInterest.End 
                                                    else segOf segmentOfInterest.Start newPortSeg.Start 
                                                else segment)
    if segmentIndex = 0 then [newPortSeg; segOf newPortSeg.End newPortSeg.End] @ modifiedSegments
    else modifiedSegments @ [segOf newPortSeg.Start newPortSeg.Start; newPortSeg]

//unused, might be useful for implementing a better auto router later
let segmentIntersectsWithSymbols segment symbolsMap =
    let symbolThatIntersectsWithSegment _symbolId symbol=
        let symbolBBox = Symbol.getSymbolBBox symbol
        let segmentBBox = getSegmentBBox segment
        if bBoxesIntersect symbolBBox segmentBBox
        then Some symbol else None

    symbolsMap |>
    Map.tryPick symbolThatIntersectsWithSegment 

// Abstract Id
type WireId = string

type WireRenderProps = {
    Segments : Segment list // the shortest possible wire with the current implementation will have two segments
    Color : CommonTypes.Color
    Width : float
    Label : string // for printing the width
    StartDir : Dir // for determining the position of the label
}                  // start dir is both a render prop and an attribute used in routing algorithms
                   
type Wire = {
    StartId: CommonTypes.PortId
    EndId: CommonTypes.PortId
    EndDir: Dir
    WireRenderProps: WireRenderProps
    }

// Some example functions for interfacing
// These should be used instead of directly accessing records, that way if a record definition is changed only these functions need to change
// Some of these have no / little use in the demo code but they should be useful later
let generateWireId () = uuid()

let getSegmentsFromWire (wire: Wire) = wire.WireRenderProps.Segments

let getStartDirFromWire wire = wire.WireRenderProps.StartDir

let getWirePropsFromWire wire = wire.WireRenderProps

let getColorFromWireProps props = props.Color

let updateWireWithProps wire props =
    if getStartDirFromWire wire <> props.StartDir then failwithf "updateWireWithProps Error: Attempted to change the wire start direction by force" // shouldn't happen
    else {wire with WireRenderProps = props}

let updateWireWithColor wire color =
    let props = {wire.WireRenderProps with Color = color}
    updateWireWithProps wire props

let updateWireWithSegments wire route =
    let props = {wire.WireRenderProps with Segments = route}
    updateWireWithProps wire props

let getStartIdFromWire wire = wire.StartId

let getEndIdFromWire wire = wire.EndId

let getEndDirFromWire wire = wire.EndDir

type Model = {
    SymbolModel : Symbol.Model
    Wires : Map<WireId,Wire>
    }

let updateWireModelWithWires wires wireModel = {wireModel with Wires = wires}

let updateWireModelWithSymbolModel symbolModel wireModel = {wireModel with SymbolModel = symbolModel}

let getWiresFromWireModel wireModel = // return a container with the wires, for this implementation it's a map
    wireModel.Wires

let getWireFromWireModel wireModel wireId =
    getWiresFromWireModel wireModel
    |> Map.find wireId
let getSymbolModelFromWireModel wireModel = wireModel.SymbolModel

let updateWireModelWithHighlightedWire model wireId color =
    let newWires = getWiresFromWireModel model
                   |> Map.map (fun wId w -> if wId <> wireId then w else updateWireWithColor w color)
    updateWireModelWithWires newWires model

let getPropsFromId model wireId =
    getWiresFromWireModel model
    |> Map.pick (fun wId w -> if wId <> wireId then None else Some (getWirePropsFromWire w))

type Msg =
    | Symbol of Symbol.Msg
    | ManualRouting of wireId : WireId * segmentIndex : int * mousePos : XYPos
    | AutoRouteAll

let singleWireView =
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let xLabel, yLabel =
                match props.StartDir with // these label positions work fine
                | Right -> props.Segments.Head.Start.X + 3., props.Segments.Head.Start.Y - 6.
                | Left -> props.Segments.Head.Start.X - 7., props.Segments.Head.Start.Y - 6.
                | Dir.Down -> props.Segments.Head.Start.X + 6., props.Segments.Head.Start.Y + 7.
                | Dir.Up -> props.Segments.Head.Start.X + 6., props.Segments.Head.Start.Y - 3.
            g[] [
                path [
                        Style[
                            StrokeLinejoin "round"
                            Fill "none"
                        ]
                        D ("M" + segmentsToString props.Segments )
                        SVGAttr.StrokeWidth props.Width
                        SVGAttr.Stroke (props.Color.Text())][]
                text[X xLabel
                     Y yLabel
                     Style[FontSize "8px"]
                         ][props.Label |> str]
                ])


let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wireSVG = 
        model.Wires
        |> Map.map (fun _wireId (wire: Wire) ->
            singleWireView wire.WireRenderProps)
        |> Map.toList
        |> List.map snd
    let symbolSVG = Symbol.view model.SymbolModel (fun symbolMsg -> dispatch (Symbol symbolMsg))
    g [] (symbolSVG :: wireSVG)

// handle wire segment movement by the user
let rec manualRouteWire segmentIndex mousePos (wireOption: Wire option) =
    let wire =
        match wireOption with
        | Some wire -> wire
        | None -> failwithf "manualRouteWire Error: Function called with None argument"
    let segments = getSegmentsFromWire wire
    let segmentOfInterest = segments.[segmentIndex]
    let segmentsMaxIdx = segments.Length - 1

    match segmentIndex = 0 || segmentIndex = segmentsMaxIdx || lenOfSeg segmentOfInterest = 0. with
    | true ->       
    // handle exceptional cases where the user wishes to change the port extension segment
    // and the highly unlikely event that the user has selected a segment of 0 length in which case dirOfSeg fails
    // if the start extension is changed then sheet needs to adjust its next drag message to wireId,2
    // this could be done internally in BusWire.fs by including a selectedItem state in the model but as a team we agreed that only sheet will have such information

        if lenOfSeg segmentOfInterest < 11. then wireOption // only split the connection if it is longer than the extension, this handles all 0 segments too!
        else                                                // note that the new segment will have length equal to the difference of the current length and the port length so we allow some extra room (i.e. < 11. instead < 10. in this case)                                
            let newSegments = splitSegmentsAtEndSeg (segmentIndex = 0) segments // split the wire further, if the user manages to drag perfectly in parallel to the segment this split will still happen
            manualRouteWire (if segmentIndex  = 0 then 2 else segmentIndex) mousePos (Some (updateWireWithSegments wire newSegments))
    | false ->
        let newMovedSegment =
            match dirOfSeg segmentOfInterest |> isHorizontalDir with // dirOfSeg is safe here
            | false -> moveSegmentToX mousePos segmentOfInterest
            | true -> moveSegmentToY mousePos segmentOfInterest
        let newSegments = segments
                          |> List.mapi (fun idx segment ->
                                            match idx - segmentIndex with
                                            | 0 -> newMovedSegment
                                            | -1 -> segOf segment.Start newMovedSegment.Start
                                            | 1 ->  segOf newMovedSegment.End segment.End
                                            | _ -> segment)
        Some (updateWireWithSegments wire newSegments)

    // fromDir is where the source pt is headed
    // Resulting wire will attack the input port from a direction perpendicular to toDir
    // Note that the exact toDir does not matter, only whether it is perpendicular to fromDir does
let rec manhattanAutoRoute fromPt fromDir toPt toDir=
    if pointsAreClose fromPt toPt && arePerependicularDirs fromDir toDir then [] // base case
    else
        let newSegment, newDir = // the new direction must always be perpendicular to fromDir
            if fromDir |> isHorizontalDir then fromPt, toPt
            else toPt, fromPt
            ||> (fun pt1 pt2 ->
                    match pointsAreCloseInDir (somePerpendicularDir fromDir) pt1 pt2 with
                    | true -> segOf fromPt fromPt, somePerpendicularDir fromDir // generate a 0 length segment, this is optimal in terms of distance but ugly
                    | false ->
                        let newSegment = segOf fromPt (posOf pt1.X pt2.Y)
                        newSegment, dirOfSeg newSegment) // dirOfSeg is safe here
        newSegment :: manhattanAutoRoute newSegment.End newDir toPt toDir

let routeFromStart startExtension endExtension =
    startExtension :: manhattanAutoRoute startExtension.End (dirOfSeg startExtension) endExtension.Start (dirOfSeg endExtension) @ [endExtension] 



// expecting a portmap from symbol people with all the ports of the symbol that moved (so just an interface function to get the ports from a symbol)
// the exact structure (list or map) does not matter as we can easily convert from one to the other
// this function will get called whenever a change is made on ports that affects the wiring
// currently the only anticipated change is ports moving (due to symbols moving)
// adjusting to port dir change is similar and can be included here if symbol decides to allow this
// other changes that could be easily implemented here are wire colour and width changes (if symbol allows width changes for example)
// a seperate function might have to be written for processing width inference changes (updating labels)
// as of the demo this is purely used for routing
let autoRouteWires wires portsMap =

    let correctEndPtsAndAutoRoute (wire: Wire) ids =
        // if ports can change positions a similar function to correctEndPt could be written, correndEndDir
        // any additional changes could also be factored in here as they would only result in a change in the WireRenderProps of the affected wire
        let correctEndPt ids connectedPortId isOutput =
            match ids with
            | id, None -> // one end exists within the port map and the other doesn't
                if id = connectedPortId then CommonTypes.getPosFromPort (Map.find connectedPortId portsMap)
                else 
                    let segments = getSegmentsFromWire wire
                    if isOutput then segments.Head.Start else (List.rev segments).Head.End
            | _ -> CommonTypes.getPosFromPort (Map.find connectedPortId portsMap) // both ends exist within the port map (same symbol)

        let startId = getStartIdFromWire wire
        let endId = getEndIdFromWire wire
        // get the new starting points of the wires
        let routeStart = correctEndPt ids startId true
        let routeEnd = correctEndPt ids endId false
        // setup for routing
        let fromDir, toDir = getStartDirFromWire wire, getEndDirFromWire wire
        let startExtension = CommonTypes.getPortExtension routeStart fromDir true
        let endExtension = CommonTypes.getPortExtension routeEnd toDir false
        let prevSegments = getSegmentsFromWire wire
        let prevSegmentsLength = prevSegments.Length
        // if the wire is long then wire memory is kept and we don't route from the beginning
        if prevSegmentsLength > 3 && snd ids = None then
            let portId = fst ids
            let segments =
                let maybeStretchedPort = 
                    if portId = endId then (List.rev prevSegments).Head
                    else prevSegments.Head
                if lenOfSeg maybeStretchedPort > 10. then                 // if the user has stretched the port we would like to preserve that part,
                    splitSegmentsAtEndSeg (portId = startId) prevSegments // in that case split the segment
                else prevSegments
            let segmentsMaxIdx = segments.Length - 1
            let autoRouteToPt =
                                if portId = endId then segments.[segmentsMaxIdx - 2].Start
                                else segments.[2].End
            let routeSoFar = segments
                             |> List.indexed
                             |> List.filter (fun (idx,_segment) ->
                                                    if portId = endId then idx < segmentsMaxIdx - 2
                                                    else idx > 2)
                             |> List.map snd
                             |> (fun r -> if portId = endId then swapRoute r else r)
            let betweenRoute =  // exact toDir used in manhattanAutoRoute doesn't matter, the below works because by construction segments will alternate Dirs (i.e. Horizontal -> Vertical -> Horizontal...)
                                if portId = endId then manhattanAutoRoute endExtension.Start (getOppositeDir toDir) autoRouteToPt (somePerpendicularDir toDir)
                                else manhattanAutoRoute startExtension.End fromDir autoRouteToPt (somePerpendicularDir fromDir)
            let route =
                        if portId = endId then (swapSeg endExtension) :: betweenRoute @ routeSoFar |> swapRoute
                        else startExtension :: betweenRoute @ routeSoFar
            updateWireWithSegments wire route
        else if prevSegmentsLength > 3 && snd ids <> None then // when dealing with a loop, simple translation of the path is done
            let route = translateRoute routeStart prevSegments // note that prevSegmentsLength > 3 can be omitted here
            updateWireWithSegments wire route
        else // route from the beginning if the wire is short
            let route = routeFromStart startExtension endExtension
            updateWireWithSegments wire route

    let connectedToPorts wire =
        let matchingStartOrEnd portId _port=
            if getStartIdFromWire wire = portId || getEndIdFromWire wire = portId
            then Some portId else None
        match Map.tryPick matchingStartOrEnd portsMap with
        | Some x ->
            (x,
             portsMap // Handles the exceptional case of a wire being connected to only one symbol (feedback loop)
             |> Map.filter (fun pId _p -> pId <> x)
             |> Map.tryPick matchingStartOrEnd)
             |> Some
        | _ -> None
    wires
    |> Map.map (fun _wireId wire ->
                match connectedToPorts wire with
                | Some ids -> correctEndPtsAndAutoRoute wire ids
                | None -> wire)

// assumes wire has been initialised
let autoRouteWire _wireId wire =
    let prevSegments = getSegmentsFromWire wire
    let startPos, endPos = prevSegments.Head.Start, (List.rev prevSegments).Head.End
    let fromDir, toDir = getStartDirFromWire wire, getEndDirFromWire wire
    let route =
        (CommonTypes.getPortExtension startPos fromDir true,
         CommonTypes.getPortExtension endPos toDir false)
        ||> routeFromStart  
    updateWireWithSegments wire route

// example function for wire creation from ports
// for custom wires with props decided by the user use the updateWireWithProps function
let createWire startId startPort endId endPort =
    // setup for the router
    let startPortPos, endPortPos = CommonTypes.getPosFromPort startPort, CommonTypes.getPosFromPort endPort
    let fromDir, toDir = CommonTypes.getDirFromPort startPort, CommonTypes.getDirFromPort endPort
    let startExtension = CommonTypes.getPortExtension startPortPos fromDir true
    let endExtension = CommonTypes.getPortExtension endPortPos toDir false
    let segments = routeFromStart startExtension endExtension
    let portWidth = // for determining wire width and color
        if CommonTypes.getWidthFromPort startPort <> CommonTypes.getWidthFromPort endPort then -1
        else CommonTypes.getWidthFromPort startPort
    {StartId = startId                                  
     EndId = endId                                          
     WireRenderProps = {Segments = segments
                        Color = if portWidth < 1 then CommonTypes.Color.Red else CommonTypes.Color.Green
                        Width = if portWidth > 1 then 2.5 else 1.5
                        Label = if portWidth < 1 then "" else sprintf "%d" portWidth
                        StartDir = fromDir}
     EndDir = toDir
    }

// dummy init for demo
let init () =
    let initSymbolModel, symbolCmd = Symbol.init () // for the demo, direct access to symbol model is fine here
    let startPorts = initSymbolModel
                    |> Map.map (fun sId s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> Map.filter (fun pId p -> CommonTypes.getPortTypeFromPort p = CommonTypes.PortType.Output))
                    |> Map.toList
                    |> List.map snd
                    |> List.collect Map.toList
    let endPorts = initSymbolModel
                    |> Map.map (fun sId s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> Map.filter (fun pId p -> CommonTypes.getPortTypeFromPort p = CommonTypes.PortType.Input))
                    |> Map.toList
                    |> List.map snd
                    |> List.collect Map.toList
    let rng = System.Random 0
    let wireMap = 
                endPorts
                |> List.map (fun eP ->
                                let idx = rng.Next(4)
                                generateWireId(), createWire (fst startPorts.[idx]) (snd startPorts.[idx]) (fst eP) (snd eP)) 
                |> Map.ofList
    {SymbolModel = initSymbolModel; Wires = wireMap}, Cmd.map Symbol symbolCmd




let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol (Symbol.MoveSymbol (symbolId,_mousePos)) ->
        let symbolMsg =
            match msg with
            | Symbol x -> x
            | _ -> failwithf "Can't happen"
        let newSymbolModel,newSymbolCmd = Symbol.update symbolMsg model.SymbolModel
        let movedPortsMap = Symbol.getPortsFromId symbolId newSymbolModel
        let newWires = autoRouteWires model.Wires movedPortsMap
        model
        |> updateWireModelWithWires newWires
        |> updateWireModelWithSymbolModel newSymbolModel, Cmd.map Symbol newSymbolCmd
    | ManualRouting (wireId,segmentIndex,mousePos) ->
        let newWires = getWiresFromWireModel model
                       |> Map.change wireId (manualRouteWire segmentIndex mousePos)
        updateWireModelWithWires newWires model, Cmd.none
    | AutoRouteAll ->
        let newWires = getWiresFromWireModel model
                       |> Map.map autoRouteWire
        model
        |> updateWireModelWithWires newWires, Cmd.none
// Bounding Box function for sheet
// could be optimised
let wireHit mousePos wireModel =
    let wireMap = getWiresFromWireModel wireModel
    let getWireBBoxes _wireId wire =
        let segments = getSegmentsFromWire wire
        List.map getSegmentBBox segments
    let selectedSegmentOnWire wireId wire=
        let selectedSegment = getWireBBoxes wireId wire
                              |> List.tryFindIndex (isInsideBBox mousePos)
        match selectedSegment with
        | Some segmentIndex -> Some (wireId,segmentIndex)
        | _ -> None

    wireMap
    |> Map.tryPick selectedSegmentOnWire