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


let symbolPorts (width : float) (height : float) (ports: Port list) (portNum : int)=
    let pName = ports.[portNum].PortName
    let yPosition = float(portNum+1)/float (List.length ports + 1 ) * height 

    

    text [ 
                    if ports.[portNum].PortType = PortType.Input then X 3. 
                    else 
                        X ( width - 3.)
                    Y yPosition 
                    Style [
                        
                        if ports.[portNum].PortType = PortType.Input then TextAnchor "left"
                        else
                            TextAnchor "end"
                        DominantBaseline "middle" 
                        FontSize "10px"
                        FontWeight "Bold"
                        Fill "black" 
                    ]
                ] [str <| sprintf "%A" pName ] 




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
    | StartDragging of sId :  SymbolId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId :  SymbolId * pagePos: XYPos
    | EndDragging of sId :  SymbolId
    | AddCircle of XYPos // used by demo code to add a circle
    | DeleteSymbol of sId: SymbolId 
    | UpdateSymbolModelWithComponent of Component // Issie interface

    | AddComp of compType: ComponentType * label: string * pagePos: XYPos


//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let fstof3 (a, _, _) = a


let secondof3 (_, b, _) = b


let thirdof3 (_, _, c) = c



//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//
//createNewSymbol functions
let makeNameId (compType: ComponentType) :string*float*float =
    match compType with 
    | Not -> ("1",20.,20.)
    | And ->("&",20.,20.)
    | Or -> ("≥1",20.,20.)
    | Xor -> ("=1",20.,20.)
    | Nand -> ("$",20.,20.)
    | Nor -> ("≥1",20.,20.)
    | Xnor -> ("=1",20.,20.)
    | Decode4 -> ("decode",40.,7.)
    | BusSelection(outWidth,outputLSBit) ->($"[{outWidth+outputLSBit-1}..{outputLSBit}]",25.,25.)
    | BusCompare (busWidth,compareWith) ->($"={compareWith}",25.,25.)

    |_-> (" ",25.,25.)

let addSymbolValues  (compType: ComponentType) =

    match compType with 
    | Not | And | Or | Xor | Nand | Nor | Xnor  -> (40.,40.,makeNameId compType)
    | Decode4 -> (80.,120.,makeNameId compType)

    |_-> (100.,100.,(" ",25.,25.))

//to make triangles and polygons of non-square shapes
let makePolygon   (compType: ComponentType) (w:float) (h:float) =
    match compType with 

    | Not | Nand | Nor | Xnor  -> (w,h/2.0),(w,h/2.0+10.0),(w+10.0,h/2.0)
    |_-> (0.,0.),(0.,0.),(0.,0.)


let createNewSym  (compType: ComponentType) (label: string) (pagePos: XYPos)=
    let tempId = Helpers.uuid()
    let (w,h,nameid) = addSymbolValues compType
    let (t1,t2,t3) = makePolygon compType w h

    {   
        //same for all Symbols
        Id =  SymbolId (tempId)
        Pos = pagePos
        LastDragPos = {X=0. ; Y=0.}
        IsDragging = false 
        Label = label
        Type = compType

        //mustchange for each symbol
        
        H = h
        W = w
        NameId = nameid

        InputPorts = [
            {
            Id = Helpers.uuid() 
            PortPos = posOf 10. 0.
            BusWidth = 1
            PortNumber = Some 0
            PortType = PortType.Input
            PortName = "IN"
            HostId = tempId
            PortDirection = Left
            }
            ]
        OutputPorts = [
            {
            Id = Helpers.uuid() 
            PortPos = posOf 10. 0.
            BusWidth = 1
            PortNumber = Some 0
            PortType = PortType.Output
            PortName = "OUT"
            HostId = tempId
            PortDirection = Right
            }
            ]
    }




/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with 
    | AddComp (compType, label ,pagePos) ->
        createNewSym  compType label pagePos ::model,Cmd.none



    // | AddCircle pos -> 
    //     createNewSym pos :: model, Cmd.none
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



let init () =
    let testcomp: ComponentType = ComponentType.Decode4
    

    let model = 
        List.allPairs [1..2] [1..2]
        |> List.map (fun (x,y) -> {X = float (x*100+30); Y=float (y*160+30)})
        |> List.map (createNewSym testcomp "Circ")
    let mode: Model = model 
    
    let model1,cmd1 = update (AddComp (  (ComponentType.And) ,"Circ", posOf 100. 10.) ) (model : Model)
    let model2,cmd2 = update (AddComp (  (ComponentType.Nor) ,"Circ", posOf 200. 20.) ) (model1 : Model)
    let model3,cmd3 = update (AddComp (  (ComponentType.Xor) ,"Circ", posOf 300. 30.) ) (model2 : Model)
    update (AddComp (  (ComponentType.Not) ,"Circ", posOf 0. 0.) ) (model3 : Model)

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCompProps =
    {
        Sym : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderComponent =
    FunctionComponent.Of(
        fun (props : RenderCompProps) ->
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
                    "yellow"
                else
                    "grey"

            let x = props.Sym.Pos.X
            let y = props.Sym.Pos.Y
            let height = props.Sym.H
            let width = props.Sym.W

            let scaleFactor=1.0 
            let rotation=0 

            g   [ Style [ 
                TransformOrigin "0px 50px" 
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " x y rotation scaleFactor )
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
                                
                                StartDragging (props.Sym.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )
                            SVGAttr.Points $"{0},{0} {width},{0} {width},{height} {0},{height}" 
                            SVGAttr.StrokeWidth "1.5px"
                            SVGAttr.Stroke "black"
                            SVGAttr.FillOpacity 0.8
                            SVGAttr.Fill color] []
                    text [
                        let (nameid,x,y) = props.Sym.NameId
                        X x; 
                        Y y;
                        Style [
                            TextAnchor "middle" 
                            DominantBaseline "middle" 
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Black" 
                        ]
                    ] [str <| sprintf $"{fstof3(props.Sym.NameId)}" ]
            ] @ List.map (symbolPorts  width height props.Sym.InputPorts) [0..(List.length props.Sym.InputPorts-1)]
            @ List.map (symbolPorts  width height props.Sym.OutputPorts) [0..(List.length props.Sym.OutputPorts-1)])
    , "Sym"
    , equalsButFunctions
    )
 




/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id =  SymbolId id} as symbol) ->
        renderComponent 
            {
                Sym = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId:  SymbolId) : XYPos = 
    List.find (fun (sym:Symbol) -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)



/// Update the symbol with matching  SymbolId to comp, or add a new symbol based on comp.
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
        (sId:CommonTypes. SymbolId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"