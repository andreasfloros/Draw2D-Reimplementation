module BusWire

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Implementation--------------------//
//------------------------------------------------------------------------//

// The Segment and Dir types are used extensively here, see Helpers module for details on these

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

// function called on ctrl click on segment, generalised version of above (could be merged in to one function)
let splitSegments (mousePos: XYPos) (segments: Segment list) (segmentIndex: int)=
    let segment = segments.[segmentIndex]
    if lenOfSeg segment <> 0. then
        let posToSplit =
            match dirOfSeg segment |> isHorizontalDir with
            | true -> posOf mousePos.X segment.Start.Y
            | false -> posOf segment.Start.X mousePos.Y
        segments
        |> List.mapi (fun id seg ->
                            if id = segmentIndex 
                            then [segOf seg.Start posToSplit; segOf posToSplit posToSplit; segOf posToSplit seg.End]
                            else [seg])
        |> List.collect id
    else segments

// unused, might be useful for implementing a better auto router later
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

// Props to be passed to the viwewing function for a single wire
// Some of these are also used in the routing algorithms
// Sgegments should probably be changed to an array for better performance
type WireRenderProps = {
    Segments : Segment list // the shortest possible wire with the current implementation will have two segments
    Color : string
    Width : float
    IsSelected : bool
    Label : string // for printing the width
    StartDir : Dir // for determining the position of the label
}                  // start dir is both a render prop and an attribute used in routing algorithms

// The wire type                   
type Wire = {
    StartId: CommonTypes.PortId
    EndId: CommonTypes.PortId
    EndDir: Dir
    HasBeenManualRouted: bool
    WireRenderProps: WireRenderProps
    }

// Some example functions for interfacing
// These should be used instead of directly accessing records, that way if a record definition is changed only these functions need to change
// Some of these have no / little use but they should be useful for someone who wants to write a bit of code without looking too much into how BusWire does things
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

// Model definition, maps are used for wires but using a dictionary could maybe help performance a bit
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

// The msg type
// SelectEnclosed might be redudant here since symbol already a similar message
type Msg =
    | Symbol of Symbol.Msg
    | ManualRouting of wireId : WireId * segmentIndex : int * mousePos : XYPos
    | DeleteWire of wireId: WireId
    | SplitSegment of wireId : WireId * segmentIndex : int * mousePos : XYPos
    | Select of wireId : WireId
    | MultipleSelect of wireId : WireId
    | AutoRouteAll
    | AutoRouteWire of wireId: WireId
    | CreateWire of port1 : CommonTypes.Port * port2 : CommonTypes.Port
    | CreateSheetWire of port : CommonTypes.Port Option * pos : XYPos
    | DeleteSheetWire
    | SelectEnclosed of p1: XYPos * p2: XYPos

// for the SelectEnclosed msg
let isInSelectionBox box wire = 
    let wireBoxes wire =    
        let segments = getSegmentsFromWire wire 
        List.map getSegmentBBox segments 
    let selectedSegment = wireBoxes wire 
    let isSelected = List.map (fun s -> bBoxesIntersect s box) selectedSegment
    if List.forall(fun x -> x = true) isSelected then true else false 

// controls visible on selection vertices
let addVerticesIfSelected props =
    if props.IsSelected then
        props.Segments
        |> List.mapi (fun index segment -> if index <> props.Segments.Length-1 then
                                                let middlePointX = segment.End.X
                                                let middlePointY = segment.End.Y
                                                let topLeft = string (middlePointX-5.5) + "," + string (middlePointY-5.5) + " "
                                                let topRight = string (middlePointX+5.5) + "," + string (middlePointY-5.5) + " "
                                                let bottomLeft = string (middlePointX-5.5) + "," + string (middlePointY+5.5) + " "
                                                let bottomRight = string (middlePointX+5.5) + "," + string (middlePointY+5.5) + " "
                                                let vertexCoordinates = topLeft + topRight + bottomRight + bottomLeft

                                                [polygon
                                                    [
                                                    SVGAttr.Points (vertexCoordinates)
                                                    SVGAttr.StrokeWidth "1px"
                                                    SVGAttr.Stroke "Black"
                                                    SVGAttr.FillOpacity 1.0
                                                    SVGAttr.Fill "Grey"] []]
                                           else
                                                [])
        |> List.concat
    else
        []


// the view function for a single wire
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
                        SVGAttr.Stroke (props.Color)][]

                text [  X xLabel
                        Y yLabel
                        Style [
                                FontSize "14px"
                                FontWeight "Bold"
                                Fill (props.Color)
                                UserSelect UserSelectOptions.None
                              ]
                         ][props.Label |> str]

                ] @ addVerticesIfSelected props))

// Wire view function
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
                                    Stroke "#0000ff"
                                    StrokeWidth "2px"
                                    StrokeDasharray "5"
                                  ]] []]
        | None -> []
    g [] (wireSVG @ sheetWire @ [symbolSVG])

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

        if lenOfSeg segmentOfInterest < portLength + 10. then wireOption // only split the connection if it is longer than the extension, this handles all 0 segments too!
        else                                                // note that the new segment will have length equal to the difference of the current length and the port length so we allow some extra room                            
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
// Note that the exact fromDir, toDir does not matter, only whether they are horizontal or vertical directions
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

let routeFromStart startExtension endExtension fromDir toDir =
    startExtension :: manhattanAutoRoute startExtension.End fromDir endExtension.Start toDir @ [endExtension] 


// condition to check if there is badness caused by symbols facing oppsite directions
let pointsAreOnRightSide pt1 dir1 pt2 dir2 = // pt1 is output pt2 is input
    if areOppositeDirs dir1 dir2 |> not then false
    else
        match dir1 |> isHorizontalDir with
        | true -> (if dir1 = Right then pt1.X - pt2.X > -2. * portLength else pt2.X - pt1.X > -2. * portLength)
        | false -> (if dir1 = Dir.Down then pt1.Y - pt2.Y > -2. * portLength else pt2.Y - pt1.Y > -2. * portLength)
// post processing of the wire to remove 0 length segments, could probably be merged with the other post processsing functions
// or, better, could be done during wire generation but this probably won't be straight forawrd
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

// very similar to above, checks for other badness condition   
let removeKinkySegs wire =
    let segments = getSegmentsFromWire wire
    let tripletToBeEradicated = 
        let segmentsMaxIdx = segments.Length - 1
        segments // could optimise...
        |> List.indexed
        |> List.rev
        |> List.tryPick (fun (idx,seg) ->
                                if idx > segmentsMaxIdx - 2 then None
                                else if lenOfSeg seg <> 0. && lenOfSeg segments.[idx+2] <> 0. && lenOfSeg segments.[idx+1] = 0. && areOppositeDirs (dirOfSeg seg) (dirOfSeg segments.[idx+2])
                                then
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

// very similar to above, checks for other badness condition
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

// this function will get called whenever a change is made on ports that affects the wiring
// this includes moving symbols as well as rotating symbols
// a seperate function might have to be written for processing width inference changes (updating labels)
let autoRouteWires wires portsMap =

    // correct wire end points and directions according to the portsMap and route the wire
    let correctEndPtsAndAutoRoute (wire: Wire) ids =

        // find the correct end point
        let correctEndPt ids connectedPortId isOutput =
            match ids with
            | id, None -> // one end exists within the port map and the other doesn't
                if id = connectedPortId then Symbol.getPosFromPort (Map.find connectedPortId portsMap)
                else 
                    let segments = getSegmentsFromWire wire
                    if isOutput then segments.Head.Start else (List.rev segments).Head.End
            | _ -> Symbol.getPosFromPort (Map.find connectedPortId portsMap) // both ends exist within the port map (same symbol)

        // find the correct direction
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
        
        // originally extensions were just the first and last segments of the wire but this was changed to include the badness case of pointsAreOnRighSide
        let startExtension, endExtension, shouldChangeDirs =
            let portExtendAtOutput, portExtendAtInput = getPortExtension routeStart fromDir true, getPortExtension routeEnd toDir false
            if pointsAreOnRightSide routeStart fromDir routeEnd toDir
            then
                let dirForExtraSeg, dist = // near output
                    match fromDir |> isHorizontalDir with
                    | true -> (if routeStart.Y > routeEnd.Y then Dir.Up else Dir.Down), abs(routeStart.Y - routeEnd.Y) / 2.
                    | false -> (if routeStart.X > routeEnd.X then Left else Right), abs(routeStart.Y - routeEnd.X) / 2.
                [portExtendAtOutput; getNovelPortExtension portExtendAtOutput.End dirForExtraSeg true dist],
                [getNovelPortExtension portExtendAtInput.Start (getOppositeDir dirForExtraSeg) false dist; portExtendAtInput],
                true
            else
                [portExtendAtOutput],[portExtendAtInput],false
        
        // previous segments are used in the HasBeenManualRouted setting
        let prevSegments = getSegmentsFromWire wire
        let prevSegmentsLength = prevSegments.Length

        // proper port extensions
        let extensionOutput, extensionInput = startExtension.Head, (List.rev endExtension).Head

        // if the wire is long then wire memory is kept and we don't route from the beginning
        if prevSegmentsLength > 2 && snd ids = None && wire.HasBeenManualRouted && oldFromDir = fromDir && oldToDir = toDir then
            let portId = fst ids
            let segments =
                let maybeStretchedPort = 
                    if portId = endId then (List.rev prevSegments).Head
                    else prevSegments.Head
                if lenOfSeg maybeStretchedPort > portLength then          // if the user has stretched the port we would like to preserve that part,
                    splitSegmentsAtEndSeg (portId = startId) prevSegments // in that case split the segment
                else prevSegments
            let segmentsMaxIdx = segments.Length - 1
            let autoRouteToPt =
                                if portId = endId then segments.[segmentsMaxIdx - 1].Start
                                else segments.[1].End
            let routeSoFar = segments
                             |> List.indexed
                             |> List.filter (fun (idx,_segment) ->
                                                    if portId = endId then idx < segmentsMaxIdx - 1
                                                    else idx > 1)
                             |> List.map snd
                             |> (fun r -> if portId = endId then swapRoute r else r)
            let betweenRoute =  // exact toDir used in manhattanAutoRoute doesn't matter, the below works because by construction segments will alternate Dirs (i.e. Horizontal -> Vertical -> Horizontal...)
                                if portId = endId then manhattanAutoRoute extensionInput.Start (getOppositeDir toDir) autoRouteToPt (oldToDir)
                                else manhattanAutoRoute extensionOutput.End (fromDir) autoRouteToPt (oldFromDir)
            let route =
                        if portId = endId then (swapSeg extensionInput) :: betweenRoute @ routeSoFar |> swapRoute
                        else extensionOutput :: betweenRoute @ routeSoFar
            updateWireWithSegments wire route
        else if prevSegmentsLength > 3 && snd ids = None && wire.HasBeenManualRouted then
            let portId = fst ids
            let segments =
                let maybeStretchedPort = 
                    if portId = endId then (List.rev prevSegments).Head
                    else prevSegments.Head
                if lenOfSeg maybeStretchedPort > portLength then          // if the user has stretched the port we would like to preserve that part,
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
                                if portId = endId then manhattanAutoRoute endExtension.Head.Start (getOppositeDir toDir |> (if shouldChangeDirs then somePerpendicularDir else id)) autoRouteToPt (somePerpendicularDir oldToDir)
                                else manhattanAutoRoute (List.rev startExtension).Head.End (fromDir |> (if shouldChangeDirs then somePerpendicularDir else id)) autoRouteToPt (somePerpendicularDir oldFromDir)
            let route =
                        if portId = endId then (swapRoute endExtension) @ betweenRoute @ routeSoFar |> swapRoute
                        else startExtension @ betweenRoute @ routeSoFar
            updateWireWithSegments wire route
        // Route translation is problematic with directions that change
        else if prevSegmentsLength > 2 && snd ids <> None && fromDir = oldFromDir && toDir = oldToDir then // when dealing with a loop, simple translation of the path is done
            let route = translateRoute routeStart prevSegments // note that prevSegmentsLength > 3 can be omitted here
            updateWireWithSegments wire route
        else // route from the beginning
            let route =
                
                if shouldChangeDirs then
                    extensionOutput :: routeFromStart (List.rev startExtension).Head endExtension.Head (somePerpendicularDir fromDir) (somePerpendicularDir toDir) @ [extensionInput]
                else routeFromStart extensionOutput extensionInput fromDir toDir
            updateWireWithSegments wire route

    let connectedToPorts wire =
        let matchingStartOrEnd portId _port=
            if getStartIdFromWire wire = portId || getEndIdFromWire wire = portId
            then Some portId else None
        match Map.tryPick matchingStartOrEnd portsMap with
        | Some x ->
            (x,
             portsMap // Handles the exceptional case of a wire being connected to only one symbol (feedback loop) and more generally multiple symbol movement
             |> Map.filter (fun pId _p -> pId <> x)
             |> Map.tryPick matchingStartOrEnd)
             |> Some
        | _ -> None
    wires
    |> Map.map (fun _wireId wire -> // for every wire in the wire map...
                match connectedToPorts wire with // check if it is connected to any of the ports in the port map
                | Some ids -> correctEndPtsAndAutoRoute wire ids // if it is then adjust the route
                              |> remove0Segs    // filter (there's probably a better way to do this)
                              |> removeKinkySegs // filter (there's probably a better way to do this)
                              |> mergeSegs  //filter (there's probably a better way to do this)
                | None -> wire) // do nothing if the wire doesn't need to be routed

// assumes wire has been initialised
// could maybe be combined with other auto routing functions as code here is pretty repetitive
let autoRouteWire _wireId wire =
    let prevSegments = getSegmentsFromWire wire
    let startPortPos, endPortPos = prevSegments.Head.Start, (List.rev prevSegments).Head.End
    let fromDir, toDir = getStartDirFromWire wire, getEndDirFromWire wire
    let startExtension, endExtension, shouldChangeDirs =
            let portExtendAtOutput, portExtendAtInput = getPortExtension startPortPos fromDir true, getPortExtension endPortPos toDir false
            if pointsAreOnRightSide startPortPos fromDir endPortPos toDir
            then
                let dirForExtraSeg, dist = // near output
                    match fromDir |> isHorizontalDir with
                    | true -> (if startPortPos.Y > endPortPos.Y then Dir.Up else Dir.Down), abs(startPortPos.Y - endPortPos.Y) / 2. 
                    | false -> (if startPortPos.X > endPortPos.X then Left else Right), abs(startPortPos.X - endPortPos.X) / 2.
                [portExtendAtOutput; getNovelPortExtension portExtendAtOutput.End dirForExtraSeg true dist],
                [getNovelPortExtension portExtendAtInput.Start (getOppositeDir dirForExtraSeg) false dist; portExtendAtInput],
                true
            else
                [portExtendAtOutput],[portExtendAtInput],false
    let segments =
        let extensionOutput, extensionInput = startExtension.Head, (List.rev endExtension).Head
        if shouldChangeDirs then
            extensionOutput :: routeFromStart (List.rev startExtension).Head endExtension.Head (somePerpendicularDir fromDir) (somePerpendicularDir toDir) @ [extensionInput]
        else routeFromStart extensionOutput extensionInput fromDir toDir
    {updateWireWithSegments wire segments with HasBeenManualRouted = false}
    |> remove0Segs
    |> removeKinkySegs
    |> mergeSegs

// example function for wire creation from ports
// ports can be given in any order (input/output, output/input)
// fails if input/input or output/output is given (sheet currently prevents this)
// some of the code here is repretitive and has to do with routing so could be written better
let createWire startId startPort endId endPort =
    // setup for the router
    let startPort, startId, endPort, endId =
            if Symbol.getPortTypeFromPort startPort <> Symbol.getPortTypeFromPort endPort then
                if Symbol.getPortTypeFromPort startPort = CommonTypes.PortType.Output
                then startPort, startId, endPort, endId else endPort, endId, startPort, startId
            else
                failwithf "createWire Error: Attempted to connect input/input, output/output"

    let startPortPos, endPortPos = Symbol.getPosFromPort startPort, Symbol.getPosFromPort endPort
    let fromDir, toDir = Symbol.getDirFromPort startPort, Symbol.getDirFromPort endPort 
    let startExtension, endExtension, shouldChangeDirs =
            let portExtendAtOutput, portExtendAtInput = getPortExtension startPortPos fromDir true, getPortExtension endPortPos toDir false
            if pointsAreOnRightSide startPortPos fromDir endPortPos toDir
            then
                let dirForExtraSeg, dist = // near output
                    match fromDir |> isHorizontalDir with
                    | true -> (if startPortPos.Y > endPortPos.Y then Dir.Up else Dir.Down), abs(startPortPos.Y - endPortPos.Y) / 2. 
                    | false -> (if startPortPos.X > endPortPos.X then Left else Right), abs(startPortPos.X - endPortPos.X) / 2.
                [portExtendAtOutput; getNovelPortExtension portExtendAtOutput.End dirForExtraSeg true dist],
                [getNovelPortExtension portExtendAtInput.Start (getOppositeDir dirForExtraSeg) false dist; portExtendAtInput],
                true
            else
                [portExtendAtOutput],[portExtendAtInput],false
    let segments =
        let extensionOutput, extensionInput = startExtension.Head, (List.rev endExtension).Head
        if shouldChangeDirs then
            extensionOutput :: routeFromStart (List.rev startExtension).Head endExtension.Head (somePerpendicularDir fromDir) (somePerpendicularDir toDir) @ [extensionInput]
        else routeFromStart extensionOutput extensionInput fromDir toDir
    let portWidth = // for determining wire width and color
        if Symbol.getWidthFromPort startPort <> Symbol.getWidthFromPort endPort then -1 // this would probably get replaced by a more sophisticated system for determining widths
        else Symbol.getWidthFromPort startPort
    {StartId = startId                                  
     EndId = endId
     HasBeenManualRouted = false                                          
     WireRenderProps = {Segments = segments
                        Color = if portWidth < 1 then "#ff0000"
                                else if portWidth > 1 then "#9121b3"
                                else "#000000"
                        Width = if portWidth > 1 then 4.
                                else if portWidth < 1 then 3.
                                else 1.5
                        Label = if portWidth < 1 then "" else sprintf "%d" portWidth
                        StartDir = fromDir
                        IsSelected = false}
     EndDir = toDir
    }
    |> remove0Segs
    |> removeKinkySegs
    |> mergeSegs

// for the wire creation animation
// don't really need a wire type for it as we only need two points to visualise this
let sheetWire (startPort: CommonTypes.Port) (endPos: XYPos) = 
    {StartId = startPort.Id                                  
     EndId = null
     HasBeenManualRouted = false                                          
     WireRenderProps = {Segments = [{Start = startPort.PortPos; End = endPos}]
                        Color = "#00ff00"
                        Width = 1.5
                        Label = null
                        StartDir = startPort.ConnectionDirection
                        IsSelected = false}
     EndDir = startPort.ConnectionDirection
    }

// dummy init for demo, not very pretty
let init () =
    let initSymbolModel, symbolCmd = Symbol.init ()
    let initSymbols = initSymbolModel.SymModel
    
    let startPorts = initSymbols
                    |> List.map (fun s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> List.filter (fun p -> Symbol.getPortTypeFromPort p = CommonTypes.PortType.Output))
                    |> List.collect id
                    |> List.map (fun p -> (Symbol.getPortIdFromPort p, p)) 
                                        
    let endPorts = initSymbols
                    |> List.map (fun s -> 
                                        Symbol.getPortsFromSymbol s
                                        |> List.filter (fun p -> Symbol.getPortTypeFromPort p = CommonTypes.PortType.Input))
                    |> List.collect id
                    |> List.map (fun p -> (Symbol.getPortIdFromPort p, p))

    let rng = System.Random 0

    let wireMap = [(generateWireId(), createWire (fst startPorts.[3]) (snd startPorts.[3]) (fst endPorts.Head) (snd endPorts.Head))]
                  |> Map.ofList

    {SymbolModel = initSymbolModel; Wires = wireMap; SheetWire = None}, Cmd.map Symbol symbolCmd



// update function for wire
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with

    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.SymbolModel
        match sMsg with
        | Symbol.StartDragging (sId, pos) -> // deselect wires on symbol drag
            let newWires = model
                           |> getWiresFromWireModel
                           |> Map.map (fun id w -> { w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false} } )
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd

        | Symbol.Dragging pos -> // correct wiring on symbol move
            let movedPortsMap = Symbol.getPortsMapOfSelectedSymbolList sm 
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd
        | Symbol.EndDragging -> // final correction on end drag
            let movedPortsMap = Symbol.getPortsMapOfSelectedSymbolList sm 
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd
        | Symbol.MouseMove (pos,None) -> {model with SymbolModel=sm}, Cmd.map Symbol sCmd // MouseMove is for port bubbles (see symbol module)
        | Symbol.RotateSymbol sId -> // same as symbol move
            let movedPortsMap = Symbol.getPortsFromId sId sm
            let newWires = autoRouteWires model.Wires movedPortsMap
            model
            |> updateWireModelWithWires newWires
            |> updateWireModelWithSymbolModel sm, Cmd.map Symbol sCmd

        | Symbol.DeleteSymbol -> // delete symbol and wires that were attached to it
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
     
    | ManualRouting (wireId,segmentIndex,mousePos) -> // handle moving segments
        let newWires = getWiresFromWireModel model
                       |> Map.change wireId (manualRouteWire segmentIndex mousePos (getWiresFromWireModel model) wireId true)
        updateWireModelWithWires newWires model, Cmd.none
                                   
    | DeleteWire wId -> // delete specific wire
        {model with Wires = Map.filter (fun id w -> id <> wId) model.Wires } , Cmd.none

    | AutoRouteAll -> // On Ctrl W
        let newWires = getWiresFromWireModel model
                       |> Map.map autoRouteWire
        model
        |> updateWireModelWithWires newWires, Cmd.none

    | AutoRouteWire wId -> // On W on selected wire
        let newWires = getWiresFromWireModel model
                       |> Map.change wId (fun wireOpt ->
                                                match wireOpt with
                                                | Some wire -> Some (autoRouteWire wId wire)
                                                | None -> failwithf "Can't happen")
        model
        |> updateWireModelWithWires newWires, Cmd.none

    | Select wId -> // select a wire
        let sm,sCmd = Symbol.update Symbol.Deselect model.SymbolModel
        let newWires = model
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> if id = wId then 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = true}}
                                               else 
                                                    {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false}})
        updateWireModelWithWires newWires {model with SymbolModel = sm}, Cmd.none

    | MultipleSelect wId -> // quite similar to above (maybe above is redundant)
        let newWires = model
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> if id = wId then
                                                    if w.WireRenderProps.IsSelected = true then 
                                                        {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = false}}
                                                    else  
                                                        {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = true}}
                                               else 
                                                    w)
        updateWireModelWithWires newWires model, Cmd.none

    | CreateWire (port1,port2) -> // expects ports to be of opposite polarity
        let newModel = {model with SheetWire = None}
        let inputPort, outputPort = if (Symbol.getPortTypeFromPort port1) = CommonTypes.PortType.Input then port1, port2 else port2, port1
        let oldWires = getWiresFromWireModel newModel
        let newWires = 
                       match oldWires |> Map.exists (fun id w -> (w.StartId = outputPort.Id && w.EndId = inputPort.Id) || w.EndId = inputPort.Id) with // don't allow duplicates or same input wires
                       | true -> oldWires
                       | false ->
                           oldWires
                           |> Map.add (generateWireId()) (createWire port1.Id port1 port2.Id port2)
        updateWireModelWithWires newWires newModel, Cmd.none

    | SplitSegment (wireId,segmentIndex,mousePos) -> // on Ctrl Click on selected wire segment
        let newWires =
            getWiresFromWireModel model
            |> Map.change wireId (fun wireOpt ->
                                    match wireOpt with
                                    | Some w -> 
                                        let segments = getSegmentsFromWire w
                                        splitSegments mousePos segments segmentIndex
                                        |> updateWireWithSegments {w with HasBeenManualRouted = true; WireRenderProps = {w.WireRenderProps with IsSelected = true}}
                                        |> Some
                                    | None -> failwithf "Can't happen")
        updateWireModelWithWires newWires model, Cmd.none 

    | CreateSheetWire (Some port, pos) -> // for wire creation animation
        {model with SheetWire = Some (sheetWire port pos)}, (pos, Some port) |> Symbol.Msg.MouseMove |> Symbol |> Cmd.ofMsg
    | DeleteSheetWire -> // on wire creatuib animation end
        {model with SheetWire = None}, Cmd.none        
    | SelectEnclosed (p1, p2) -> // probably redundant
        let box = createSelectBox p1 p2 
        let newWires = model 
                       |> getWiresFromWireModel
                       |> Map.map (fun id w -> 
                                        if isInSelectionBox box w
                                        then {w with WireRenderProps = {getWirePropsFromWire w with IsSelected = true}}
                                        else w)  
        {model with Wires = newWires}, (p1,p2) |> Symbol.SelectEnclosed |> Symbol |> Cmd.ofMsg
                       
          

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



// currently unused
let getSelectedWireList model =
    model
    |> getWiresFromWireModel
    |> Map.filter (fun id w -> w.WireRenderProps.IsSelected = true)
    |> Map.toList
    |> List.map fst
