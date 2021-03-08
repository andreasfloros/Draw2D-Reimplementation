//This module has been written by Zaid Jafarey and is used as a stub for the Sheet demo 
//Some changes have been made by me (Anushka Kulkarni) in order to implement Sheet functionality, and those changes have been commented as "Anushka"

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
type Symbol = Component


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
    | StartDragging of sId : SymbolId * pagePos: XYPos
    // /// coords not adjusted for top-level zoom
    | Dragging of sId : SymbolId * pagePos: XYPos
    | EndDragging of sId : SymbolId
    | AddCircle of label: string * pagePos: XYPos // used by demo code to add a circle
    | AddSymbol of CompType: ComponentType * label: string * pagePos: XYPos 
    | AdddSymbol  // *My addition* - for all real components
    | DeleteSymbol of sId:SymbolId
    | Unselect of sId:SymbolId
    | UpdateSymbolModelWithComponent of Component // Issie interface
    | MultipleSelect of sId : SymbolId


//---------------------------------helper types and functions----------------//


let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
    /// 
 
//Anushka -- written for adding wires -- unused for now
let createPortBB (port: Port) (x: float) = 
    {
        TopLeft = {X = port.PortPos.X - x ; Y = port.PortPos.Y - x }
        BottomRight = {X = port.PortPos.X + x ; Y = port.PortPos.Y + x}
    }

//Anushka -- written for adding wires -- unused for now
let FindPort (mousePos: XYPos) (model: Model) = 
    let s = model |> List.map (fun sym -> 
                            match List.tryFind (fun p -> containsPoint (createPortBB p 1.) mousePos) sym.Ports with 
                            | Some p -> Some (p.PortId, p.PortType)
                            | None -> None
                        )  
    match List.filter (fun x -> x <> None) s with 
    | [] -> None 
    | s -> s.[0]
    


let createBB (sym: Symbol) (h,w: float) =
    {
        TopLeft = {X =  sym.Pos.X ; Y = sym.Pos.Y}
        BottomRight = {X =  sym.Pos.X + w ; Y = sym.Pos.Y + h}
    }

let FindSymbol (mousePos: XYPos) (model: Model) = 
    match List.tryFind (fun sym -> containsPoint (createBB sym (sym.H,sym.W)) mousePos) model with 
    | Some sym -> Some sym.Id
    | None -> None

type CompProps = {
        Id : SymbolId
        Pos : XYPos
        Type : ComponentType
        NumOfUpInputs : int
        H : float
        W : float
        HeaderMargin : float
        FooterMargin : float
    }


let generatePortList compProps numOfPorts portType connectionDirection =
    
    let busWidth portNum = 
        match compProps.Type, portType, portNum with
        | Decode4, PortType.Input, 0 -> 2
        | _ -> 1

    match portType,connectionDirection with
    
    | PortType.Input,Up -> 
        let portDists = compProps.W/float(numOfPorts+1)
        let relativePortPos i = 
            match compProps.Type with
            | Mux2 | Mux4 | MuxN _ -> {X = float(i)*portDists ; Y = compProps.H - float(i*10)}
            | Demux2 | Demux4 | DemuxN _ -> {X = float(i)*portDists ; Y = compProps.H - compProps.FooterMargin + float(i*10)}
            | _ -> {X = float(i)*portDists ; Y = compProps.H}
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                PortId = Helpers.uuid()
                PortNumber = Some (i-1)
                PortType = portType
                PortPos = {X = compProps.Pos.X + (relativePortPos i).X ; Y = compProps.Pos.Y + (relativePortPos i).Y}
                RelativePortPos = relativePortPos i
                BusWidth = (busWidth i)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
                
            })

    | PortType.Input,_ -> 
        let portDists = (compProps.H - compProps.HeaderMargin - compProps.FooterMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                PortId = Helpers.uuid()
                PortNumber = Some (i-1)
                PortType = portType
                PortPos = {X = compProps.Pos.X ; Y = compProps.Pos.Y + compProps.HeaderMargin + (float(i)*portDists)}
                RelativePortPos = {X = 0. ; Y = compProps.HeaderMargin + (float(i)*portDists)}
                BusWidth = (busWidth i)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })

    | PortType.Output,_ -> 
        let portDists = (compProps.H - compProps.HeaderMargin - compProps.FooterMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                PortId = Helpers.uuid()
                PortNumber = Some (i-1)
                PortType = portType
                PortPos = {X = compProps.Pos.X + compProps.W ; Y = compProps.Pos.Y + compProps.HeaderMargin + (float(i)*portDists)}
                RelativePortPos = {X = compProps.W ; Y = compProps.HeaderMargin + (float(i)*portDists)}
                BusWidth = (busWidth i)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })
    //| _ -> failwithf "should not occur"

/// Used to calculate the properties of any 'basic' component that does not contain 
/// any pins at the bottom of the symbol.
// To add new compnonets: add a line to the top match statement and provide the number 
// of input pins and the number of output pins. Later match statements can be used to 
// implement unique symbol design.
let generateBasicCompProps compType compId compPos =
    let ((numOfInputs:int), (numOfOutputs:int)) =
        match compType with 
        | Not -> (1, 1)
        | And | Or | Xor | Nand | Nor | Xnor -> (2, 1)
        | Decode4 -> (2, 4)
        | Input _ | Output _ | Constant _ -> (0, 1)
        | _ -> failwithf "should not occur"

    let headerMargin = 
        match compType with 
        | Decode4 -> 20.
        | _ -> 0.
    let footerMargin = 0.

    let height = 
        match compType with 
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 50.
        | _ ->  headerMargin + footerMargin + 20. + float(20 * List.max [numOfInputs; numOfOutputs])
    let width = 
        match compType with 
        | Not | Nand | Nor | Xnor -> 60.
        | Decode4 -> 80.
        | _ -> 50.  
    
    let compProps = {
        Id = compId
        Pos = compPos
        Type = compType
        NumOfUpInputs = 0
        H = height
        W = width
        HeaderMargin = headerMargin
        FooterMargin = footerMargin
    }

    let inPortList = generatePortList compProps numOfInputs PortType.Input Right 
    let outPortList = generatePortList compProps numOfOutputs PortType.Output Left  
    ((List.append inPortList outPortList), compProps.H, compProps.W, numOfInputs, numOfOutputs, 0)


    

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (compType:ComponentType) (label:string) (pos:XYPos) : Component = 
    let compId = SymbolId (Helpers.uuid())
    let (portList, height, width, numOfInputs, numOfOutputs, numOfUpInputs) = 
        match compType with
        | Not | And | Or | Xor | Nand | Nor | Xnor -> generateBasicCompProps compType compId pos 
        | Decode4 -> generateBasicCompProps compType compId pos 
        | Input bw | Output bw | Constant (bw,_) -> generateBasicCompProps compType compId pos
        | _ -> failwithf "Error: component not implemented"
    {
        Id = compId
        Type = compType
        Label = label 
        Ports = portList
        NumOfInputs = numOfInputs
        NumOfOutputs = numOfOutputs
        NumOfUpInputs = numOfUpInputs
        NumOfSelects = 0
        Pos = pos
        H = height
        W = width
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false 
        Colour = Blue
    } 

/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.map ((fun (x,y) -> {X = float (x*200+40); Y=float (y*200+40)}) >> (fun {X=x;Y=y} -> 
        if y<x 
        then (createNewSymbol (Xnor) "label" {X=x;Y=y}) 
        else (createNewSymbol (Decode4) "label" {X=x;Y=y}))) (List.allPairs [1..2] [1..3])
    , Cmd.none



/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    // | AdddSymbol pos -> (createNewSymbol (Xnor) "label" pos) :: model, Cmd.none

    | AddSymbol (compType, label, pos) -> 
        (createNewSymbol compType label pos) :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Colour <> Red) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                {sym with Colour = Blue}
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                    Colour = Green 
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
                    Colour = Red 
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
                    Colour = Red
                }
        )
        , Cmd.none

    | Unselect sId -> //Anushka
        model
        |> List.map (fun sym ->
                if sym.Id <> sId then sym 
                else {sym with Colour = Blue})
        , Cmd.none

    | MultipleSelect sId -> //Anushka
        model
        |> List.map (fun sym ->
            if sId = sym.Id then
                {sym with Colour = Red}
            else sym 
        )
        , Cmd.none
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//


type private BasicSymbolProps =
    {
        Sym : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change





let private portLabels (sym:Symbol) (i:int) =
    match sym.Type with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Input _ | Output _ -> text [] []
    | _ -> 
        let port = sym.Ports.[i]
        let (xMargin, yMargin, textAnchor, dominantBaseline) = 
            match port.ConnectionDirection with 
            | Right -> (5., 0., "start", "middle")
            | Left -> (-5., 0., "end", "middle")
            | Up -> (0., -6., "middle", "auto")
            | Down -> (0., 6., "middle", "hanging")
        let labelPrefix, labelNum =
            match sym.Type with 
            | Decode4 -> 
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "SEL", ""
                | PortType.Input, _ -> "DATA", ""
                | PortType.Output, _ -> "", string(i-sym.NumOfInputs)
            | _ ->
                match port.PortType, port.ConnectionDirection with
                | PortType.Input, Up -> "s", string(i-sym.NumOfInputs+sym.NumOfUpInputs)
                | PortType.Input, _ -> "i", string(i)
                | PortType.Output, _ -> "", string(i-sym.NumOfInputs)
            //| _ -> failwithf "Error: should not occur"

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
        ] [str <| $"{labelPrefix}{labelNum}"] 




let private renderBasicSymbol = 
    let log2 n = log (float n) / log 2.0
    FunctionComponent.Of(
        fun (props : BasicSymbolProps) ->
            // let handleMouseMove =
            //     Hooks.useRef(fun (ev : Types.Event) ->
            //         let ev = ev :?> Types.MouseEvent
            //         // x,y coordinates here do not compensate for transform in Sheet
            //         // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
            //         Dragging(props.Sym.Id, posOf ev.pageX ev.pageY)
            //         |> props.Dispatch
            //     )

            let fX = props.Sym.Pos.X
            let fY = props.Sym.Pos.Y
            let fW = 
                match props.Sym.Type with
                | Not | Nand | Nor | Xnor -> props.Sym.W - 10.
                | _ -> props.Sym.W
            let fH = props.Sym.H
            let n = List.length(props.Sym.Ports)

            let muxCut = 
                match props.Sym.Type with 
                | Mux2 -> 20.
                | Mux4 -> 30.
                | MuxN n -> 10. + float(log2 n)*10.
                | _ -> 0.

            let demuxCut = 
                match props.Sym.Type with 
                | Demux2 -> 20.
                | Demux4 -> 30.
                | DemuxN n -> 10. + float(log2 n)*10.
                | _ -> 0.

            let headerMargin = 
                match props.Sym.Type with 
                | Mux2 | Mux4 | MuxN _ -> muxCut + 5.
                | Demux2 | Demux4 | DemuxN _ -> demuxCut + 5.
                | _ -> 15.

            let header = 
                match props.Sym.Type with
                | Not -> "1"
                | And | Nand -> "&"
                | Or | Nor -> ">=1"
                | Xor | Xnor -> "=1"
                | Decode4 -> "Decode"
                | Mux2 | Mux4 | MuxN _ -> "Mux"
                | Demux2 | Demux4 | DemuxN _ -> "Demux"
                | _ -> "symbol not recognized"


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
                 

                            SVGAttr.Points $"{0.},{demuxCut} {0.},{fH-demuxCut} {fW},{fH-muxCut} {fW},{muxCut}"
                            SVGAttr.StrokeWidth "2px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 0.1
                            SVGAttr.Fill (string(props.Sym.Colour))] []
        
                    text [ 
                        X (fW/2.); 
                        Y headerMargin; 
                        Style [
                            TextAnchor "middle" 
                            DominantBaseline "middle" 
                            FontSize "13px"
                            FontWeight "Bold"
                            Fill "Black" 
                        ]
                    ] [str <| sprintf $"{header}"] 

            ] @ (List.map (portLabels props.Sym) [0..n-1]) ) 

           
    , "BasicSymbol"
    , equalsButFunctions
    )


let renderSymbols (dispatch : Msg -> unit) (symToRender : Symbol) : ReactElement  =
        match symToRender.Type with 
        | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 
        | Mux2 | Demux2 | Mux4 | Demux4 | MuxN _ | DemuxN _ -> 
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
    |> List.map (renderSymbols dispatch) 
    |> ofList


        
//---------------Other interface functions--------------------//
let symbolPos (symModel: Model) (sId: SymbolId) : XYPos = 
    List.find (fun (sym:Symbol) -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)


let getsymbolFromSymbolId (sId: SymbolId) (symModel: Model) : Symbol = 
    List.find (fun sym -> sym.Id = sId) symModel 

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:ComponentId) : Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : Component list = 
    failwithf "Not implemented"
