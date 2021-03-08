//This module has been written by Vasileios Manginas and is used as a stub for the Sheet demo 
//Some changes have been made by me (Anushka Kulkarni) in order to implement Sheet functionality, and those changes have been commented as "Anushka"

//=====================================================================================================
module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//---------------------------------Wire, Model, Msg definitions------------------------------//
// Wire record contains id, source and target ports, color, width, line vertices as
// a list of XYPositions, bounding boxes for each of the wire segments, and a boolean 
// value to indicate whether this wire has been manual routed in the past
type Wire = {
    WireId: CommonTypes.ConnectionId 
    SourcePort: CommonTypes.Port
    TargetPort: CommonTypes.Port
    WireColor: string
    WireWidth: string
    Vertices: XYPos list
    BoundingBox: BB list
    Routed : bool
}

// Model for wire contains a symbol model (aka symbol list) along with a list of wires
type Model = {
    Wires: Wire list
    Symbol: Symbol.Model
    }

// Only messages needed for Wire are of type symbol (to move symbol) as well as manualRoute
// in order to move wire segments
type Msg =
    | Symbol of Symbol.Msg
    | ManualRouting of wireId: CommonTypes.ConnectionId * sIndex: int * clickCoords: XYPos
    | StopMovingWire of wireId: CommonTypes.ConnectionId
    | DeleteWire of wireId: CommonTypes.ConnectionId
    | Reset
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | Unselect of wireId: CommonTypes.ConnectionId
    | Select of wireId : CommonTypes.ConnectionId
//-----------------------------------------------------------------------------------------//

type WireId = CommonTypes.ConnectionId

//------------------------------interface function definitions-----------------------------//
// Returns the Wire part of the BusWire model
let getWireListfromWireModel (model: Model) : Wire List = 
    model.Wires


// Returns the Symbol part of the BusWire model
let getSymbolModelFromWireModel (model: Model) = 
    model.Symbol


// Used by the wireHit function below
// Returns a boolean list stating whether a click on the canvas hit each bounding box
let clickInBBs (clickCoords: XYPos) (wire: Wire) =
    let boundingBoxFoundOption = wire.BoundingBox
                                 |> List.map (fun boundingBox -> containsPoint boundingBox clickCoords)
                                 |> List.tryFindIndex (id)
    (wire.WireId,boundingBoxFoundOption)


// Used by Sheet. Checks if a click on the canvas is on a wire
// Accepts as inputs the click coordinates as an XYPosition as well as the model itself
// and returns either the wireID and the index of the segment if the click is on the
// bounding box of a segment of that wire  or None if it is not
let findWire (clickCoords: XYPos) (model: Model) = 
    let wireIdSegmentIndex = model.Wires
                             |> List.map (fun wire -> clickInBBs clickCoords wire)
                             |> List.filter (fun (x,y) -> y <> None)

    if List.isEmpty wireIdSegmentIndex then 
        None
    else
        let wireSegment = wireIdSegmentIndex.[0]
        match wireSegment with
        | (wireId,segmentIndex) -> match segmentIndex with 
                                   | Some x -> Some (wireId,x)
                                   | _      -> failwithf "Don't care, never happens"
//-----------------------------------------------------------------------------------------//



//-------------------------------helping function definitions------------------------------//
// Global portExtensionDistance
let portExtensionDistance = 40.


// Returns a wire given a wireID
let wireFromWireId (wireId: CommonTypes.ConnectionId) (model: Model) =
    model.Wires
        |> List.find (fun wire -> wire.WireId = wireId)


// Computes the bounding boxes of all of a wire's segments given its vertices
// Allowance = 5 pixels above and below the wire
let createBoundingBoxes (verticesList: XYPos list) : BB list = 
    verticesList
    |> List.pairwise
    |> List.map (fun (xy1,xy2)  ->  if xy1.X <= xy2.X then
                                        {
                                            TopLeft     = {X = xy1.X-10.   ;   Y = xy1.Y-10.}
                                            BottomRight = {X = xy2.X+10.   ;   Y = xy2.Y+10.} 
                                        }
                                    else
                                        {
                                            TopLeft     = {X = xy2.X-10.   ;   Y = xy2.Y-10.}
                                            BottomRight = {X = xy1.X+10.   ;   Y = xy1.Y+10.} 
                                        }
                                    )


// Computes the default vertices of a line between a given source port and a given
// target port. This basically depends on whether the source port is on the left or  
// on the right of the target port
let defaultVertices (sourcePort: CommonTypes.Port) (targetPort: CommonTypes.Port) : XYPos list = 
    let x1 = sourcePort.PortPos.X
    let y1 = sourcePort.PortPos.Y
    let x2 = targetPort.PortPos.X
    let y2 = targetPort.PortPos.Y
    let p  = portExtensionDistance

    if (x1 + 2.*p) < x2 then
        [   {X = x1           ;     Y = y1} 
            {X = (x1+x2)/2.   ;     Y = y1}
            {X = (x1+x2)/2.   ;     Y = y2}
            {X = x2           ;     Y = y2}
        ]
    else
        [   {X = x1     ;   Y = y1} 
            {X = x1+p   ;   Y = y1}
            {X = x1+p   ;   Y = (y1+y2)/2.}
            {X = x2-p   ;   Y = (y1+y2)/2.}
            {X = x2-p   ;   Y = y2}
            {X = x2     ;   Y = y2}
        ]


// Used by the renderConnection function below
// Transforms the vertices list (aka XYPos list) into a string for svg polyline
let verticesToString (verticesList: XYPos list) : string = 
    verticesList
    |> List.map (fun vertex -> string(vertex.X) + "," + string(vertex.Y) + " ")
    |> List.fold (+) ""


// Used by the singleWireView function below
// Renders a polyline given the vertices as a string as well as the wire's attributes
let renderConnection (wire: Wire) (verticesString: string) = 
    let widthLineXCoord = wire.Vertices.[0].X + portExtensionDistance/2.
    let widthLineY1 = wire.Vertices.[0].Y - 10.
    let widthLineY2 = wire.Vertices.[0].Y + 10.
    let widthLineCoordString = (string widthLineXCoord) + "," + (string widthLineY1)
                               + " " + (string widthLineXCoord) + "," + (string widthLineY2)
    let widthText = (string wire.SourcePort.BusWidth)


    g[]
        [
            polyline [
                SVGAttr.Points verticesString
                SVGAttr.StrokeWidth wire.WireWidth
                SVGAttr.Stroke wire.WireColor
                SVGAttr.Fill "None"
            ] []

            polyline [
                SVGAttr.Points widthLineCoordString
                SVGAttr.StrokeWidth wire.WireWidth
                SVGAttr.Stroke wire.WireColor
                SVGAttr.Fill "None"
            ] []

            

            text [ 
                X widthLineXCoord; 
                Y (widthLineY1 - 25.); 
                Style [
                    TextAnchor "middle"
                    DominantBaseline "hanging"
                    FontSize "22px"
                    FontWeight "Bold"
                    Fill wire.WireColor
                ]
            ] [str <| widthText]

        ]


// Renders a single wire
let singleWireView = 
    FunctionComponent.Of(
        fun (props: Wire) ->
                let stringVertices = verticesToString props.Vertices
                renderConnection props stringVertices
            )


// Used in the updateWire function below
// Updates a wire that has been routed in the past by moving only the segments
// connected to the moving symbol
let updateRouted (wire: Wire) (symbol: Symbol.Symbol) = 
    let portid = symbol.Ports.[0].PortId
    if wire.SourcePort.PortId = portid then
        let newPortPos = symbol.Ports.[0].PortPos
        let previousVertices = wire.Vertices
        let newVertices = previousVertices
                          |> List.mapi (fun index value -> if index = 0 then
                                                               newPortPos
                                                           elif index = 1 then
                                                               {
                                                                   X = previousVertices.[1].X
                                                                   Y = newPortPos.Y
                                                               } 
                                                           else 
                                                               value)
        {wire with Vertices = newVertices
                   BoundingBox = createBoundingBoxes newVertices
                   WireColor = string(CommonTypes.Grey)}
    else
        let newPortPos = symbol.Ports.[0].PortPos
        let previousVertices = wire.Vertices
        let lastIndex = List.length previousVertices - 1
        let newVertices = previousVertices
                          |> List.mapi (fun index value -> if index = lastIndex then
                                                               newPortPos
                                                           elif index = lastIndex - 1 then
                                                               {
                                                                   X = previousVertices.[index].X
                                                                   Y = newPortPos.Y
                                                               }
                                                           else 
                                                               value)
        {wire with Vertices = newVertices ;
                   BoundingBox = createBoundingBoxes newVertices
                   WireColor = string(CommonTypes.Grey)}


// Used in the updateWire function below
// Updates a wire that hasn't been routed in the past and so redoes default
// routing given the new port positions
let updateNotRouted (wire: Wire) (symbol: Symbol.Symbol) = 
    let portid = symbol.Ports.[0].PortId
    if wire.SourcePort.PortId = portid then
        let newSourcePort = symbol.Ports.[0]
        let newVertices = defaultVertices newSourcePort wire.TargetPort
        {wire with SourcePort = newSourcePort
                   Vertices = newVertices
                   BoundingBox = createBoundingBoxes newVertices
                   WireColor = string(CommonTypes.Grey)}

    else
        let newTargetPort = symbol.Ports.[0]
        let newVertices = defaultVertices wire.SourcePort newTargetPort
        {wire with TargetPort = newTargetPort
                   Vertices = newVertices
                   BoundingBox = createBoundingBoxes newVertices
                   WireColor = string(CommonTypes.Grey)
                   }


// Used in the update function under message moveSymbol
// Calls the two functions above on all wires affected by the moved symbol
// and leaves the unaffected ones unchanged
let updateWire (sId: CommonTypes.SymbolId) (wire: Wire) (model: Model) =
    let symbol = Symbol.getsymbolFromSymbolId sId model.Symbol
    let portid = symbol.Ports.[0].PortId
    
    if (wire.SourcePort.PortId = portid) || (wire.TargetPort.PortId = portid) then
        if wire.Routed then 
            updateRouted wire symbol
        else
            updateNotRouted wire symbol
    else
        wire


// Used in the manualRoute function below
// Returns a boolean stating whether the given segment of wire is horizontal
// by checking whether its two endpoint vertices have the same Y coordinate
let segmentIsHorizontal (sIndex: int) (wire: Wire) = 
    wire.Vertices.[sIndex].Y = wire.Vertices.[sIndex+1].Y


// Used in the ipdate function under message ManualRouting
// Routes one segment of a wire given the coordinates of the click on canvas
// If segment is horizontal, only the y coords of its two endpoints are changed
// If segment is vertical, only the x coords of its two endpoints are changed
let manualRoute (sIndex: int) (wire: Wire) (clickCoords: XYPos) = 
    let finalSegmentIndex = wire.BoundingBox.Length - 1
    
    if sIndex=0 || sIndex=finalSegmentIndex then
        wire
    else 
        if segmentIsHorizontal sIndex wire then
            let newVertices = wire.Vertices
                              |> List.mapi (fun index value -> if (index = sIndex) || (index = sIndex+1) then
                                                                    {
                                                                        X = value.X 
                                                                        Y = clickCoords.Y
                                                                    }
                                                                else
                                                                    value)
            {wire with Vertices = newVertices
                       BoundingBox = createBoundingBoxes newVertices
                       WireColor = "Red"
                       Routed = true}
        else
            let newVertices = wire.Vertices
                              |> List.mapi (fun index value -> if (index = sIndex) || (index = sIndex+1) then
                                                                    {
                                                                        X = clickCoords.X 
                                                                        Y = value.Y
                                                                    }
                                                                else
                                                                    value)
            {wire with Vertices = newVertices
                       BoundingBox = createBoundingBoxes newVertices
                       WireColor = "Red"
                       Routed = true}


//-----------------------------------------------------------------------------------------//

let view (model:Model) (dispatch: Dispatch<Msg>) =
    printfn "%A" model.Wires
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    let renderedWires = List.map (singleWireView) model.Wires
    g [] [(g [] renderedWires); symbols]



let init () =
    let symbols, cmd = Symbol.init()
    let combinations = [ (symbols.[0],symbols.[1]) ; (symbols.[3],symbols.[4]) ]

    combinations
    |> List.map (fun (s1,s2) -> let srcPort = s1.Ports.[0]
                                let tgtPort = s2.Ports.[0]
                                let initialVertices = defaultVertices srcPort tgtPort
                                {
                                    WireId = CommonTypes.ConnectionId (uuid())
                                    SourcePort = srcPort
                                    TargetPort = tgtPort
                                    WireColor = if srcPort.BusWidth = 1 then
                                                    "Black"
                                                else
                                                    "Blue"
                                    WireWidth = if srcPort.BusWidth = 1 then
                                                    "2px"
                                                else
                                                    "6px"
                                    Vertices = initialVertices
                                    BoundingBox = createBoundingBoxes initialVertices
                                    Routed = false
                                    
                                })
    |> (fun wires -> {Wires=wires; Symbol=symbols}, Cmd.none)



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with

    | Symbol sMsg -> //Anushka 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let newModel = {model with Symbol = sm}
        match sMsg with
        | Symbol.StartDragging (sId, pos) -> 
                    let updatedWires = 
                        model.Wires
                        |> List.map (fun wire -> updateWire sId wire newModel)
                    {newModel with Wires=updatedWires}, Cmd.map Symbol sCmd
        | Symbol.Dragging (sId,pos) -> 
                    let updatedWires= 
                        model.Wires
                        |> List.map (fun wire -> updateWire sId wire newModel)
                    {newModel with Wires=updatedWires}, Cmd.map Symbol sCmd
        | Symbol.EndDragging sId -> newModel, Cmd.map Symbol sCmd
        | Symbol.DeleteSymbol sId -> 
            {newModel with Wires = (List.filter (fun w -> w.SourcePort.HostId <> sId && w.TargetPort.HostId <> sId) model.Wires)}, 
                                       Cmd.map Symbol sCmd
        | Symbol.AddSymbol (sym, label, pos) -> 
            {model with Symbol = sm}, Cmd.map Symbol sCmd
        | Symbol.Unselect sId -> newModel, Cmd.map Symbol sCmd
        | Symbol.MultipleSelect sId -> newModel, Cmd.map Symbol sCmd
        | _ -> failwithf "not yet done"
        
    | ManualRouting (wId,sIndex,clickCoords) -> let newWires =  model.Wires
                                                              |> List.map (fun wire -> if wId <> wire.WireId then
                                                                                            wire
                                                                                       else
                                                                                            manualRoute sIndex wire clickCoords)
                                                {model with Wires=newWires}, Cmd.none 
                                                
    | StopMovingWire wId -> let newWires =  model.Wires
                                            |> List.map (fun wire -> if wId <> wire.WireId then
                                                                        {wire with WireColor = string(CommonTypes.Grey)}
                                                                     else
                                                                        if wire.SourcePort.BusWidth = 1 then
                                                                            {wire with WireColor = string(CommonTypes.Red) }
                                                                        else    
                                                                            {wire with WireColor = string(CommonTypes.Blue)})
                            {model with Wires=newWires}, Cmd.none

    | Reset -> let newWires =  model.Wires
                               |> List.map (fun wire -> let newVertices = defaultVertices wire.SourcePort wire.TargetPort
                                                        {wire with Vertices = newVertices
                                                                   BoundingBox = createBoundingBoxes newVertices
                                                                   Routed = false
                                                                   WireColor = string(CommonTypes.Grey)
                                                                   })
               {model with Wires=newWires}, Cmd.none

    | SetColor c -> {model with Wires = List.map (fun w -> {w with WireColor = string(c)}) model.Wires}, Cmd.none //Anushka

    | Unselect wId -> //Anushka
                let w = model.Wires |> List.map (fun w ->  {w with WireColor = string(CommonTypes.Grey)})
                {model with Wires = w}, Cmd.none
                                   
    | DeleteWire wId -> //Anushka
        {model with Wires = List.filter (fun w -> w.WireColor <> string(CommonTypes.Red)) model.Wires}, Cmd.none

    | Select wId -> //Anushka
        let w = model.Wires |> List.map (fun w -> 
                            if w.WireId = wId then {w with WireColor = string(CommonTypes.Red)}
                            else {w with WireColor = string(CommonTypes.Grey)})
        {model with Wires = w}, Cmd.none

 
   
                      


    



