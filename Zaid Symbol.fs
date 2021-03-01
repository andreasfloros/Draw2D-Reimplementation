module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.
type Symbol = CommonTypes.Component


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.SymbolId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId : CommonTypes.SymbolId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.SymbolId
    | AddCircle of label: string * pagePos: XYPos // used by demo code to add a circle
    | AddSymbol of CompType: CommonTypes.ComponentType * label: string * pagePos: XYPos // *My addition* - for all real components
    | DeleteSymbol of sId:CommonTypes.SymbolId
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

let generatePortList compId compPos height width headerMargin numOfPorts portType selectPin busWidth =
    match portType,selectPin with
    | PortType.Input,false -> 
        let portDists = (height - headerMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun x -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (x-1)
                PortType = portType
                SelectPin = false
                PortPos = {X = compPos.X ; Y = compPos.Y + headerMargin + (float(x)*portDists)}
                RelativePortPos = {X = 0. ; Y = headerMargin + (float(x)*portDists)}
                BusWidth = busWidth
                ConnectionDirection = Right
                HostId = compId
            })
    | PortType.Output,false -> 
        let portDists = (height - headerMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun x -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (x-1)
                PortType = portType
                SelectPin = false
                PortPos = {X = compPos.X + width ; Y = compPos.Y + headerMargin + (float(x)*portDists)}
                RelativePortPos = {X = width ; Y = headerMargin + (float(x)*portDists)}
                BusWidth = busWidth
                ConnectionDirection = Left
                HostId = compId
            })
    | PortType.Input,true -> 
        let portDists = width/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun x -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (x-1)
                PortType = portType
                SelectPin = true
                PortPos = {X = compPos.X + (float(x)*portDists) ; Y = compPos.Y + height}
                RelativePortPos = {X = float(x)*portDists ; Y = height}
                BusWidth = busWidth
                ConnectionDirection = Up
                HostId = compId
            })
    | _ -> failwithf "should not occur"

/// Used to calculate the properties of any 'basic' component that does not contain 
/// select pins and can have ports with varying BusWidths.
/// To add new compnonets: add a line to the match statement and provide the number 
/// of input pins and the number of output pins.
let generateBasicCompProps compType compId compPos busWidth =
    let ((numOfInputs:int), (numOfOutputs:int)) =
        match compType with 
        | Not -> (1, 1)
        | And | Or | Xor | Nand | Nor | Xnor -> (2, 1)
        | Decode4 -> (4, 16)
        | Input _ | Output _ | Constant _ -> (0, 1)
        | _ -> failwithf "should not occur"

    let headerMargin = 0.
    let height = headerMargin + 20. + float(20 * List.max [numOfInputs; numOfOutputs])
    let width = 40.
    // let inPortList =                    // manual implementation of a inPortList - use if automated version (generatePortList) doesn't work right
    //    [1..numOfInputs] 
    //     |> List.map (fun x -> 
    //         {
    //             Id = Helpers.uuid()
    //             PortNumber = Some (x-1)
    //             PortType = PortType.Input
    //             PortPos = {X = compPos.X + 2. ; Y = compPos.Y - (float(x)*20.)}
    //             BusWidth = Some 1
    //             ConnectionDirection = Right
    //             HostId = compId
    //         })
    // let outPortList =                    // manual implementation of a outPortList    
    //     [1..numOfOutputs] 
    //     |> List.map (fun x -> 
    //         {
    //             Id = Helpers.uuid()
    //             PortNumber = Some (x-1)
    //             PortType = PortType.Output
    //             BusWidth = Some 1
    //             ConnectionDirection = Left
    //             HostId = compId
    //         })     
    let inPortList = generatePortList compId compPos height width headerMargin numOfInputs PortType.Input false (Some busWidth) 
    let outPortList = generatePortList compId compPos height width headerMargin numOfOutputs PortType.Output false (Some busWidth)  
    ((List.append inPortList outPortList), height, width, numOfInputs, numOfOutputs, 0)


/// Used to calculate the properties of any multiplexer-like component that contains 
/// select pins and only has pins of BusWidth = 1.
/// To add new compnonets: add a line to the match statement and provide the number 
/// of (non-select) input pins, the number of select pins and the number of output pins.
let generateMuxProps compType compId compPos =
    let ((numOfDataInputs:int), (numOfSelects:int), (numOfOutputs:int)) =
        match compType with 
        | Mux2 -> (2, 1, 1)
        | Demux2 -> (1, 1, 2)
        | Mux4 -> (4, 2, 1)
        | Demux4 -> (1, 2, 4)
        | _ -> failwithf "should not occur"

    let headerMargin = 20.
    let height = headerMargin + 20. + float(20 * List.max [numOfDataInputs; numOfOutputs])
    let width = 25. + float(25*numOfSelects)

    let inPortList = generatePortList compId compPos height width headerMargin numOfDataInputs PortType.Input false (Some 1) 
    let selectPortList = generatePortList compId compPos height width headerMargin numOfSelects PortType.Input true (Some 1) 
    let outPortList = generatePortList compId compPos height width headerMargin numOfOutputs PortType.Output false (Some 1)   
    ((inPortList @ selectPortList @ outPortList), height, width, (numOfDataInputs+numOfSelects), numOfOutputs, numOfSelects)
    

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (compType:CommonTypes.ComponentType) (label:string) (pos:XYPos) : CommonTypes.Component = 
    let compId = SymbolId (Helpers.uuid())
    let (portList, height, width, numOfInputs, numOfOutputs, numOfSelects) = 
        match compType with
        | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 -> generateBasicCompProps compType compId pos 1
        | Input bw | Output bw | Constant (bw,_) -> generateBasicCompProps compType compId pos bw
        | Mux2 | Demux2 | Mux4 | Demux4 -> generateMuxProps compType compId pos
        | _ -> failwithf "Error: component not implemented"
    {
        Id = compId
        Type = compType
        Label = label 
        Ports = portList
        NumOfInputs = numOfInputs
        NumOfOutputs = numOfOutputs
        NumOfSelects = numOfSelects
        Pos = pos
        H = height
        W = width
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false 
    } 

/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..3] [1..3]
    |> List.map (fun (x,y) -> {X = float (x*180+60); Y=float (y*180+60)})
    |> List.map (createNewSymbol CommonTypes.Mux4 "label")
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddCircle (label, pos) -> 
        (createNewSymbol CommonTypes.Circle label pos) :: model, Cmd.none 
    | AddSymbol (compType, label, pos) -> 
        (createNewSymbol compType label pos) :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pagePos
                }
        )
        , Cmd.none

    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Sym : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

type private BasicSymbolProps =
    {
        Sym : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderCircle =
    FunctionComponent.Of(
        fun (props : RenderCircleProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Sym.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Sym.IsDragging then
                    "green"
                else
                    "grey"

            circle
                [ 
                    OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Sym.Id
                        |> props.Dispatch
                    )
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDragging (props.Sym.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )
                    Cx props.Sym.Pos.X
                    Cy props.Sym.Pos.Y
                    R 20.
                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 1
                ]
                [ ]

                
    , "Circle"
    , equalsButFunctions
    )


let portLabels (sym:Symbol) (i:int) =
    match sym.Type with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Input _ | Output _ -> text [] []
    | _ -> 
        let port = sym.Ports.[i]
        let (xMargin, yMargin, textAnchor, dominantBaseline) = 
            match port.ConnectionDirection with 
            | Right -> (2., 0., "start", "middle")
            | Left -> (-2., 0., "end", "middle")
            | Up -> (0., -2., "middle", "auto")
            | Down -> (0., 2., "middle", "hanging")
        let labelPrefix, labelNum =
            match port.PortType, port.SelectPin with
            | PortType.Input, false -> "i", i
            | PortType.Output, false -> "q", (i-sym.NumOfInputs)
            | PortType.Input, true -> "s", (i-sym.NumOfInputs+sym.NumOfSelects)
            | _ -> failwithf "Error: should not occur"

        text [ 
            X (port.RelativePortPos.X + xMargin); 
            Y (port.RelativePortPos.Y + yMargin); 
            Style [
                TextAnchor textAnchor
                DominantBaseline dominantBaseline
                FontSize "13px"
                FontWeight "Bold"
                Fill "Black" 
            ]
        ] [str <| sprintf $"{labelPrefix}{labelNum}"]

// let portLabels (sym:Symbol) (i:int) =
//         text [ 
//             X 0.; 
//             Y 10.; 
//             Style [
//                 TextAnchor "miiddle"
//                 DominantBaseline "middle"
//                 FontSize "13px"
//                 FontWeight "Bold"
//                 Fill "Black" 
//             ]
//         ] [str <| sprintf $"hello"]


let private renderBasicSymbol = 
    FunctionComponent.Of(
        fun (props : BasicSymbolProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Sym.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let fX = props.Sym.Pos.X
            let fY = props.Sym.Pos.Y
            let fW = props.Sym.W
            let fH = props.Sym.H
            let n = List.length(props.Sym.Ports)

            let header = 
                match props.Sym.Type with
                | Not -> "1"
                | And | Nand -> "&"
                | Or | Nor -> ">=1"
                | Xor | Xnor -> "=1"
                | Mux2 | Mux4 -> "Mux"
                | Demux2 | Demux4 -> "Demux"
                | _ -> "symbol not recognized"

            let color =
                if props.Sym.IsDragging then
                    "blue"
                else
                    "gray"

            let scaleFactor = 1.0
            let rotation = 0

            g   [ Style [ 
                TransformOrigin "0px 50px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
                ]
                ]
 
                ([

                    polygon 
                        [
                            OnMouseUp (fun ev -> 
                                document.removeEventListener("mousemove", handleMouseMove.current)
                                EndDragging props.Sym.Id
                                |> props.Dispatch
                            )
                            OnMouseDown (fun ev -> 
                                // See note above re coords wrong if zoom <> 1.0
                                StartDragging (props.Sym.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )

                            SVGAttr.Points $"{0.},{0.} {0.},{fH} {fW},{fH} {fW},{0.}"
                            SVGAttr.StrokeWidth "2px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 0.1
                            SVGAttr.Fill color] []
                    
                    // line [X1 0.; Y1 0.; X2 0.; Y2 (100.) ; Style [Stroke "Black"]] [
                    //  // child elements of line do not display since children of svg are dom elements
                    //  // and svg will only display on svg canvas, not in dom.
                    //  // hence this is not used
                    // ]
                    text [ // a demo text svg element
                        // X (fX + (fW/2.)); 
                        // Y (fY + 15.); 
                        X (fW/2.); 
                        Y 15.; 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "13px"
                            FontWeight "Bold"
                            Fill "Black" // demo font color
                        ]
                    //] [str <| sprintf "X=%.0f Y=%.0f" fX fY] // child of text element is text to display
                    ] [str <| sprintf $"{header}"]

            ] @ List.map (portLabels props.Sym) [0..n-1]) 

           
    , "Circle?"
    , equalsButFunctions
    )


let renderSymbols (dispatch : Msg -> unit) (symToRender : Symbol) : ReactElement  =
        match symToRender.Type with 
        | CommonTypes.Circle -> 
            (renderCircle 
            {
                Sym = symToRender
                Dispatch = dispatch
                Key = string(symToRender.Id)
            })
        | Not | And | Or | Xor | Nand | Nor | Xnor | Mux2 | Demux2 | Mux4 | Demux4 -> 
            (renderBasicSymbol 
            {
                Sym = symToRender
                Dispatch = dispatch
                Key = string(symToRender.Id)
            })
        | _ -> failwithf "not implemented other symbols"


/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) : ReactElement = 
    model
    // |> List.map (fun ({Id = CommonTypes.ComponentId id} as circle) ->
    |> List.map (renderSymbols dispatch) 
    //     renderCircle 
    //         {
    //             Circle = circle
    //             Dispatch = dispatch
    //             Key = id
    //         }
    // )
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.SymbolId) : XYPos = 
    List.find (fun (sym:Symbol) -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)



/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

