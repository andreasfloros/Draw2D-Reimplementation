module BusWire

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
        |||> getPortExtension 
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
    IsSelected : bool
    Label : string // for printing the width
    StartDir : Dir // for determining the position of the label
}                  // start dir is both a render prop and an attribute used in routing algorithms
                   
type Wire = {
    StartId: CommonTypes.PortId
    EndId: CommonTypes.PortId
    EndDir: Dir
    HasBeenManualRouted: bool
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
    SheetWire : Wire option
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
    | DeleteWire of wireId: WireId
    | SetColor of CommonTypes.HighLightColor
    | Deselect of wireId: WireId
    | Select of wireId : WireId
    | MultipleSelect of wireId : WireId
    | AutoRouteAll
    | CreateWire of outputPort : CommonTypes.Port * inputPort : CommonTypes.Port
    | CreateSheetWire of port : CommonTypes.Port * pos : XYPos
    | DeleteSheetWire

let addVerticesIfSelected props =
    if props.IsSelected then
        props.Segments
        |> List.mapi (fun index segment -> if index <> props.Segments.Length-1 then
                                                let middlePointX = segment.End.X
                                                let middlePointY = segment.End.Y
                                                let topLeft = string (middlePointX-4.) + "," + string (middlePointY-4.) + " "
                                                let topRight = string (middlePointX+4.) + "," + string (middlePointY-4.) + " "
                                                let bottomLeft = string (middlePointX-4.) + "," + string (middlePointY+4.) + " "
                                                let bottomRight = string (middlePointX+4.) + "," + string (middlePointY+4.) + " "
                                                let vertexCoordinates = topLeft + topRight + bottomRight + bottomLeft

                                                [polygon
                                                    [
                                                    SVGAttr.Points (vertexCoordinates)
                                                    SVGAttr.StrokeWidth "1px"
                                                    SVGAttr.Stroke "Black"
                                                    SVGAttr.FillOpacity 0.9
                                                    SVGAttr.Fill "Grey"] []]
                                           else
                                                [])
        |> List.concat
    else
        []



let singleWireView =
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let (xLabel, yLabel) =
                match props.StartDir with // these label positions work fine
                | Right -> props.Segments.Head.Start.X + 9., props.Segments.Head.Start.Y - 6.
                | Left -> props.Segments.Head.Start.X - 19., props.Segments.Head.Start.Y - 6.
                | Dir.Down -> props.Segments.Head.Start.X + 6., props.Segments.Head.Start.Y + 20.
                | Dir.Up -> props.Segments.Head.Start.X + 6., props.Segments.Head.Start.Y - 8.
            g[] ([
                path [
                        Style [
                                StrokeLinejoin "round"
                                Fill "none"
                              ]

                        let firstSegmentStart = posToString props.Segments.Head.Start
                        D ("M " + firstSegmentStart + segmentsToRoundedString props.Segments) // for rounded corners, below line is normal
                        //D ("M " + segmentsToString props.Segments)
                        SVGAttr.StrokeWidth props.Width
                        SVGAttr.Stroke (if props.IsSelected then "Red" else props.Color.Text())][]

                text [  X xLabel
                        Y yLabel
                        Style [
                                FontSize "14px"
                                FontWeight "Bold"
                                Fill (if props.IsSelected then "Red" else props.Color.Text())
                                UserSelect UserSelectOptions.None
                              ]
                         ][props.Label |> str]

                ] @ addVerticesIfSelected props))


let view (model:Model) (dispatch: Dispatch<Msg>)=

    let wireSVG = 
        model.Wires
        |> Map.map (fun _wireId (wire: Wire) ->
            singleWireView wire.WireRenderProps)
        |> Map.toList
        |> List.map snd
    let symbolSVG = Symbol.view model.SymbolModel (fun symbolMsg -> dispatch (Symbol symbolMsg))
    let sheetWire =
        match model.SheetWire with
        | Some x -> [line [
                            X1 x.WireRenderProps.Segments.Head.Start.X
                            Y1 x.WireRenderProps.Segments.Head.Start.Y
                            X2 x.WireRenderProps.Segments.Head.End.X
                            Y2 x.WireRenderProps.Segments.Head.End.Y
                            Style [
                                    Stroke "Black"
                                    StrokeWidth "3px"
                                    StrokeDasharray "8"
                                  ]] []]
        | None -> []
    g [] (symbolSVG :: wireSVG @ sheetWire)

// handle wire segment movement by the user
let rec manualRouteWire segmentIndex mousePos wires wireId snap (wireOption: Wire option)=
    
    let wire =
        match wireOption with
        | Some wire -> wire
        | None -> failwithf "manualRouteWire Error: Function called with None argument"
    let segments = getSegmentsFromWire wire
    let segmentOfInterest = segments.[segmentIndex]
    let segmentsMaxIdx = segments.Length - 1



    // called post manual routing
    let snapToWire wire =
        let commonOutputWires = Map.filter (fun id w -> getStartIdFromWire w = getStartIdFromWire wire) wires
        let updatedSegmentOfInterest = (getSegmentsFromWire wire).[segmentIndex]
        let snapSegment =
            let closeEnoughSegment id w =
                if lenOfSeg updatedSegmentOfInterest = 0. || segmentIndex = 0 || segmentIndex = segmentsMaxIdx then None // could optimise here
                else
                    getSegmentsFromWire w
                    |> List.indexed
                    |> List.tryPick (fun (idx,segx) ->
                                            if lenOfSeg segx = 0. || (id = wireId && segmentIndex = idx) then None
                                            else
                                                if segmentsAreClose updatedSegmentOfInterest segx then
                                                    Some (idx,segx)
                                                else None)
            Map.tryPick closeEnoughSegment commonOutputWires
        match snapSegment with
        | Some (idx,segx) ->
            match manualRouteWire segmentIndex segx.Start wires wireId false (Some wire) with
            | Some x -> 
                printfn "Snapped to %d" idx // Debuggig message
                x
            | _ -> failwithf "Can't happen"
        | None -> wire


    match segmentIndex = 0 || segmentIndex = segmentsMaxIdx || lenOfSeg segmentOfInterest = 0. with
    | true ->       
    // handle exceptional cases where the user wishes to change the port extension segment
    // and the highly unlikely event that the user has selected a segment of 0 length in which case dirOfSeg fails
    // if the start extension is changed then sheet needs to adjust its next drag message to wireId,2
    // this could be done internally in BusWire.fs by including a selectedItem state in the model but as a team we agreed that only sheet will have such information

        if lenOfSeg segmentOfInterest < portLength + 1. then wireOption // only split the connection if it is longer than the extension, this handles all 0 segments too!
        else                                                // note that the new segment will have length equal to the difference of the current length and the port length so we allow some extra room (i.e. < 11. instead < 10. in this case)                                
            let newSegments = splitSegmentsAtEndSeg (segmentIndex = 0) segments // split the wire further, if the user manages to drag perfectly in parallel to the segment this split will still happen
            manualRouteWire (if segmentIndex  = 0 then 2 else segmentIndex) mousePos wires wireId snap (Some (updateWireWithSegments wire newSegments))
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
        Some ({updateWireWithSegments wire newSegments with HasBeenManualRouted = true} |> if snap then snapToWire else id)

    // fromDir is where the source pt is headed
    // Resulting wire will attack the input port from a direction perpendicular to toDir
    // Note that the exact toDir does not matter, only whether it is perpendicular to fromDir does
let rec manhattanAutoRoute fromPt fromDir toPt toDir=
    if pointsAreClose fromPt toPt (10. ** (-9.)) && arePerependicularDirs fromDir toDir then [] // base case
    else
        let newSegment, newDir = // the new direction must always be perpendicular to fromDir
            if fromDir |> isHorizontalDir then fromPt, toPt
            else toPt, fromPt
            ||> (fun pt1 pt2 ->
                    match pointsAreCloseInDir (somePerpendicularDir fromDir) pt1 pt2 (10. ** (-9.)) with
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
    let remove0Segs wire =
        let segments = getSegmentsFromWire wire
        let tripletToBeEradicated = 
            let segmentsMaxIdx = segments.Length - 1
            segments // could optimise...
            |> List.indexed
            |> List.tryPick (fun (idx,seg) ->
                                    if idx > segmentsMaxIdx - 2 then None
                                    else if lenOfSeg seg = 0. && lenOfSeg segments.[idx+1] = 0. && lenOfSeg segments.[idx+2] = 0.
                                    then 
                                        printfn "0 0 0  FOUND" 
                                        (Some idx) 
                                    else None)

        match tripletToBeEradicated with    
        | Some index ->
            segments
            |> List.mapi (fun idx seg ->
                                if idx = index || idx = index + 2 then []
                                else [seg])
            |> List.collect id
            |> updateWireWithSegments wire
        | None -> wire
        
    let removeKinkySegs wire =
        let segments = getSegmentsFromWire wire
        let tripletToBeEradicated = 
            let segmentsMaxIdx = segments.Length - 1
            segments // could optimise...
            |> List.indexed
            |> List.tryPick (fun (idx,seg) ->
                                    if idx > segmentsMaxIdx - 3 || idx < 1 then None
                                    else if lenOfSeg seg <> 0. && lenOfSeg segments.[idx+2] <> 0. && lenOfSeg segments.[idx+1] = 0. && areOppositeDirs (dirOfSeg seg) (dirOfSeg segments.[idx+2])
                                    then
                                        printfn "OPPOSITE FOUND" 
                                        (Some idx)
                                    else None)

        match tripletToBeEradicated with
        | Some index ->
            segments
            |> List.mapi (fun idx seg ->
                                if idx = index || idx = index + 1 then []
                                else if idx = index + 2 then [segOf segments.[index].Start seg.End]
                                else [seg])
            |> List.collect id
            |> updateWireWithSegments wire      
        | None -> wire

    let mergeSegs wire =
        let segments = getSegmentsFromWire wire
        let tripletToBeEradicated = 
            let segmentsMaxIdx = segments.Length - 1
            segments // could optimise...
            |> List.indexed
            |> List.tryPick (fun (idx,seg) ->
                                    if idx > segmentsMaxIdx - 2  then None
                                    else if lenOfSeg seg <> 0. && lenOfSeg segments.[idx+2] <> 0. && lenOfSeg segments.[idx+1] = 0. && (dirOfSeg seg) = (dirOfSeg segments.[idx+2])
                                    then
                                        printfn "SAME DIR FOUND" 
                                        (Some idx)
                                    else None)

        match tripletToBeEradicated with
        | Some index ->
            segments
            |> List.mapi (fun idx seg ->
                                if idx = index || idx = index + 2 then []
                                else if idx = index + 1 then [segOf segments.[index].Start segments.[index+2].End]
                                else [seg])
            |> List.collect id
            |> updateWireWithSegments wire      
        | None -> wire
        
    let correctEndPtsAndAutoRoute (wire: Wire) ids =
        // if ports can change positions a similar function to correctEndPt could be written, correndEndDir
        // any additional changes could also be factored in here as they would only result in a change in the WireRenderProps of the affected wire
        let correctEndPt ids connectedPortId isOutput =
            match ids with
            | id, None -> // one end exists within the port map and the other doesn't
                if id = connectedPortId then Symbol.getPosFromPort (Map.find connectedPortId portsMap)
                else 
                    let segments = getSegmentsFromWire wire
                    if isOutput then segments.Head.Start else (List.rev segments).Head.End
            | _ -> Symbol.getPosFromPort (Map.find connectedPortId portsMap) // both ends exist within the port map (same symbol)
        let correctEndDir ids connectedPortId isOutput =
            match ids with
            | id, None -> // one end exists within the port map and the other doesn't
                if id = connectedPortId then Symbol.getDirFromPort (Map.find connectedPortId portsMap)
                else 
                    if isOutput then getStartDirFromWire wire else getEndDirFromWire wire
            | _ -> Symbol.getDirFromPort (Map.find connectedPortId portsMap) // both ends exist within the port map (same symbol)

        let startId = getStartIdFromWire wire
        let endId = getEndIdFromWire wire
        // get the new starting points of the wires
        let routeStart = correctEndPt ids startId true
        let routeEnd = correctEndPt ids endId false
        // setup for routing
        let oldFromDir, oldToDir = getStartDirFromWire wire, getEndDirFromWire wire
        let fromDir, toDir = correctEndDir ids startId true, correctEndDir ids endId false
        let wire = {wire with EndDir = toDir; WireRenderProps = {getWirePropsFromWire wire with StartDir = fromDir}}
        let startExtension = getPortExtension routeStart fromDir true
        let endExtension = getPortExtension routeEnd toDir false
        let prevSegments = getSegmentsFromWire wire
        let prevSegmentsLength = prevSegments.Length
        printfn "SEG LENGTH %A" prevSegmentsLength
        // if the wire is long then wire memory is kept and we don't route from the beginning
        if prevSegmentsLength > 3 && snd ids = None then
            let portId = fst ids
            let segments =
                let maybeStretchedPort = 
                    if portId = endId then (List.rev prevSegments).Head
                    else prevSegments.Head
                if lenOfSeg maybeStretchedPort > portLength then                 // if the user has stretched the port we would like to preserve that part,
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
                                if portId = endId then manhattanAutoRoute endExtension.Start (getOppositeDir toDir) autoRouteToPt (somePerpendicularDir oldToDir)
                                else manhattanAutoRoute startExtension.End fromDir autoRouteToPt (somePerpendicularDir oldFromDir)
            let route =
                        if portId = endId then (swapSeg endExtension) :: betweenRoute @ routeSoFar |> swapRoute
                        else startExtension :: betweenRoute @ routeSoFar
            updateWireWithSegments wire route
        // Route translation is problematic with directions that change
        else if prevSegmentsLength > 3 && snd ids <> None && fromDir = oldFromDir && toDir = oldToDir then // when dealing with a loop, simple translation of the path is done
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
                              |> remove0Segs
                              |> removeKinkySegs
                              |> mergeSegs
                | None -> wire)

// assumes wire has been initialised
let autoRouteWire _wireId wire =
    let prevSegments = getSegmentsFromWire wire
    let startPos, endPos = prevSegments.Head.Start, (List.rev prevSegments).Head.End
    let fromDir, toDir = getStartDirFromWire wire, getEndDirFromWire wire
    let route =
        (getPortExtension startPos fromDir true,
         getPortExtension endPos toDir false)
        ||> routeFromStart  
    updateWireWithSegments wire route

// example function for wire creation from ports
// for custom wires with props decided by the user use the updateWireWithProps function
let createWire startId startPort endId endPort =
    // setup for the router
    let startPort, endPort =
            if Symbol.getPortTypeFromPort startPort <> Symbol.getPortTypeFromPort endPort then
                if Symbol.getPortTypeFromPort startPort = CommonTypes.PortType.Output
                then startPort, endPort else endPort, startPort
            else
                failwithf "createWire Error: Attempted to connect input/input, output/output"

    let startPortPos, endPortPos = Symbol.getPosFromPort startPort, Symbol.getPosFromPort endPort
    let fromDir, toDir = Symbol.getDirFromPort startPort, Symbol.getDirFromPort endPort
    let startExtension = getPortExtension startPortPos fromDir true
    let endExtension = getPortExtension endPortPos toDir false
    let segments = routeFromStart startExtension endExtension
    let portWidth = // for determining wire width and color
        if Symbol.getWidthFromPort startPort <> Symbol.getWidthFromPort endPort then 5
        else Symbol.getWidthFromPort startPort
    {StartId = startId                                  
     EndId = endId
     HasBeenManualRouted = false                                          
     WireRenderProps = {Segments = segments
                        Color = if portWidth < 1 then CommonTypes.Color.Red else CommonTypes.Color.Blue
                        Width = if portWidth > 1 then 4. else 1.5
                        Label = if portWidth < 1 then "" else sprintf "%d" portWidth
                        StartDir = fromDir
                        IsSelected = false}
     EndDir = toDir
    }

let sheetWire (startPort: CommonTypes.Port) (endPos: XYPos) = 
    {StartId = startPort.Id                                  
     EndId = null
     HasBeenManualRouted = false                                          
     WireRenderProps = {Segments = [{Start = startPort.PortPos; End = endPos}]
                        Color = CommonTypes.Color.Green
                        Width = 1.5
                        Label = null
                        StartDir = startPort.ConnectionDirection
                        IsSelected = false}
     EndDir = startPort.ConnectionDirection
    }

// dummy init for demo
let init () =
    let initSymbolModel, symbolCmd = Symbol.init () // for the demo, direct access to symbol model is fine here
    let startPorts = initSymbolModel
                    |> List.map (fun s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> List.filter (fun p -> Symbol.getPortTypeFromPort p = CommonTypes.PortType.Output))
                    |> List.collect id
                    |> List.map (fun p -> (Symbol.getPortIdFromPort p, p)) 
                                        
                    //|> Map.toList
                    //|> List.map snd
                    //|> List.collect Map.toList
    let endPorts = initSymbolModel
                    |> List.map (fun s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> List.filter (fun p -> Symbol.getPortTypeFromPort p = CommonTypes.PortType.Input))
                    |> List.collect id
                    |> List.map (fun p -> (Symbol.getPortIdFromPort p, p))
                    //|> Map.toList
                    //|> List.map snd
                    //|> List.collect Map.toList
    let rng = System.Random 0

    let wireMap = [(generateWireId(), createWire (fst startPorts.[3]) (snd startPorts.[3]) (fst endPorts.Head) (snd endPorts.Head))]
                  |> Map.ofList

    {SymbolModel = initSymbolModel; Wires = wireMap; SheetWire = None}, Cmd.map Symbol symbolCmd




let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with

    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.SymbolModel
        match sMsg with
        | Symbol.StartDragging (sId, pos) ->
            let newWires = model
                           |> getWiresFromWireModel
                           |> Map.map (fun id w -> { w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false} } )
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd

        | Symbol.Dragging pos ->
            let movedPortsMap = Symbol.getPortsMapOfSelectedSymbolList sm 
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd
        | Symbol.EndDragging -> 
            let movedPortsMap = Symbol.getPortsMapOfSelectedSymbolList sm 
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd
        | Symbol.MouseMove pos -> {model with SymbolModel=sm}, Cmd.map Symbol sCmd///// Shaheer /// for port bubbles
        | Symbol.RotateSymbol sId ->
            let movedPortsMap = Symbol.getPortsFromId sId sm
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd

        | Symbol.DeleteSymbol ->
            let deletedPorts = model 
                               |> getSymbolModelFromWireModel
                               |> Symbol.getPortsOfSelectedSymbolList
            let newWires = Map.filter (fun id w ->
                                            match List.tryFind (fun pId -> getStartIdFromWire w = pId) deletedPorts with
                                            | Some x -> false
                                            | None -> 
                                                match List.tryFind (fun pId -> getEndIdFromWire w = pId) deletedPorts with
                                                | Some x -> false
                                                | None -> true) model.Wires
            
            let newWires = Map.filter (fun id w -> w.WireRenderProps.IsSelected <> true) newWires
                                                
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd
        | _ -> {model with SymbolModel=sm},Cmd.none
     
    | ManualRouting (wireId,segmentIndex,mousePos) ->
        let newWires = getWiresFromWireModel model
                       |> Map.change wireId (manualRouteWire segmentIndex mousePos (getWiresFromWireModel model) wireId true)
        updateWireModelWithWires newWires model, Cmd.none
                                   
    | DeleteWire wId -> 
        {model with Wires = Map.filter (fun id w -> id <> wId) model.Wires } , Cmd.none

    | AutoRouteAll ->
        let newWires = getWiresFromWireModel model
                       |> Map.map autoRouteWire
        model
        |> updateWireModelWithWires newWires, Cmd.none

    | Select wId -> 
        let sm,sCmd = Symbol.update Symbol.Deselect model.SymbolModel
        let newWires = model
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> if id = wId then 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = true}}
                                               else 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false}})
        updateWireModelWithWires newWires {model with SymbolModel = sm}, Cmd.none

    | MultipleSelect wId -> 
        let newWires = model
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> if id = wId then 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = true}}
                                               else 
                                                    w)
        updateWireModelWithWires newWires model, Cmd.none
    | Deselect wId -> 
        let newWires = model
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> if id = wId then 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false}}
                                               else 
                                                    w)
        updateWireModelWithWires newWires model, Cmd.none
    | CreateWire (outputPort,inputPort) ->
        printfn "%A" (outputPort, inputPort)
        let newModel = {model with SheetWire = None}
        let newWires = newModel
                       |> getWiresFromWireModel
                       |> Map.add (generateWireId()) (createWire outputPort.Id outputPort inputPort.Id inputPort)
        updateWireModelWithWires newWires newModel, Cmd.none       
    | CreateSheetWire (port, pos) -> 
        {model with SheetWire = Some (sheetWire port pos)}, pos |> Symbol.Msg.MouseMove |> Symbol |> Cmd.ofMsg
    | DeleteSheetWire ->
        {model with SheetWire = None}, Cmd.none              
                       
          

// Bounding Box function for sheet
// could be optimised
let findWire mousePos wireModel =
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


let getSelectedWireList model =
    model
    |> getWiresFromWireModel
    |> Map.filter (fun id w -> w.WireRenderProps.IsSelected = true)
    |> Map.toList
    |> List.map fst
