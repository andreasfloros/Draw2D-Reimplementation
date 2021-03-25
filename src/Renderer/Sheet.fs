module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Box = {
        p1: XYPos 
        p2: XYPos 
    }

type SelectedItem = 
    | Symbol of symbolId: CommonTypes.SymbolId
    | BusWire of wireId: BusWire.WireId  * segmentIndex : int
    | Port of port : CommonTypes.Port * portType : CommonTypes.PortType
    | NoItem
    // | BusWire of wireId: BusWire.wireId * segmentIndex: int

 type Model = {
    Wire: BusWire.Model
    SelectedItem: SelectedItem
    Zoom: float
    SelectionBox: Box
    }

type KeyboardMsg =
    | CtrlS | AltShiftZ | DEL | A | B | C | D | E | F | G | H | I | CtrlW | W | R | CtrlPlus | CtrlMinus | X

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    

let constantGridLines = gridLines 1100 1100
//let Zoom = 1.
/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
/// 
let createBox(box : Box) =
    let p1 = box.p1
    let p2 = box.p2
    if p2.X< p1.X && p2.Y < p1.Y then 
        polygon [
            SVGAttr.Points $"{p2.X},{p2.Y} {p1.X},{p2.Y} {p1.X},{p1.Y} {p2.X},{p1.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "Blue"] []

    elif p2.X > p1.X && p2.Y < p1.Y then 
        polygon [
            SVGAttr.Points $"{p1.X},{p2.Y} {p2.X},{p2.Y} {p2.X},{p1.Y} {p1.X},{p1.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "Blue"] []
    elif p2.X < p1.X && p2.Y > p1.Y then 
        polygon [
            SVGAttr.Points $"{p2.X},{p1.Y} {p1.X},{p1.Y} {p1.X},{p2.Y} {p2.X},{p2.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "Blue"] []
    else 
        polygon [
            SVGAttr.Points $"{p1.X},{p1.Y} {p2.X},{p1.Y} {p2.X},{p2.Y} {p1.X},{p2.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "Blue"] []

let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((2000. * zoom))
    //let halfSize = "500."
    //let size = "1000."
    //let viewBoxArg = ("-" + halfSize + " " + "-" + halfSize + " " + size + " " + size)
    /// Is the mouse button currently down?
    let container = document.getElementById("Container")
    let rect = if container <> null then 
                    container.getBoundingClientRect() 
               else null
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0.
    
    let mShift (ev:Types.MouseEvent) = 
        ev.shiftKey = true
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
            dispatch <| MouseMsg {Op = op ; Pos = { X = ev.clientX + (if container <> null then 
                                                                        printfn "CONTAINER WAS NOT NULL"
                                                                        container.scrollLeft - rect.left else 0.)
                                                    Y = ev.clientY + (if container <> null then container.scrollTop - rect.top else 0.)}}
    printfn "SCROLL DOWN!"
    
    div [ Style 
            [ 
                // Height "100vh" 
                // MaxWidth "100vw"
                Height sizeInPixels
                MaxWidth sizeInPixels
                //CSSProp.OverflowX OverflowOptions.Auto 
                //CSSProp.OverflowY OverflowOptions.Auto
                OverflowStyle OverflowOptions.Scroll
            ]

          Id "Container"
          OnMouseDown (fun ev -> 
                        if mShift ev
                        then (mouseOp MouseOp.Shift ev)
                        else (mouseOp MouseOp.Down ev))



          OnMouseUp (fun ev -> (mouseOp MouseOp.Up ev))

          OnMouseMove (fun ev -> 
                        if mDown ev 
                        then (mouseOp MouseOp.Drag ev)
                        else mouseOp Move ev)
                    
    ]
        
        [ svg
            [ Style [
                        Transform (sprintf "scale(%f)" zoom)
                        Border "3px solid blue"
                        Height sizeInPixels
                        Width sizeInPixels    
                    ]   //ViewBox viewBoxArg
                ] // top-level transform style attribute for zoom


                (constantGridLines     // adds grid lines
                //@ (addMenuSymbols)
                @ [                    // adds menu
                    polygon 
                            [
                            SVGAttr.Points ("1100,0 1580,0 1580,1100 1100,1100")
                            SVGAttr.StrokeWidth "2px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 0.5
                            SVGAttr.Fill "lightgrey"] []
                  ]
                @ [svgReact])  // the application code
        ]

    


/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    let dragbox= createBox model.SelectionBox
    let combine = [wireSvg;dragbox] |> ofList
    displaySvgWithZoom  model.Zoom combine dispatch
     

    
    
    //createBox model.SelectionBox


let getHit (click: XYPos) (model: Model) =
    let sModel = BusWire.getSymbolModelFromWireModel model.Wire
    match Symbol.FindPort click sModel with 
    | Some (p,t) ->
        printf "Get hit found a port" 
        Port (p, t)
    | None -> 
        match BusWire.findWire click model.Wire with 
        | Some wId -> 
            printf "Get hit found a wire"
            BusWire wId 
        | None -> 
            match Symbol.FindSymbol click sModel with 
            | Some sId -> 
                printf "Get hit found a symbol"
                Symbol sId 

            | None -> 
                printf "Get hit found nothing"
                NoItem 

let getStringtoComponentType (s : string) = 
    match s with 
    | "Not" -> CommonTypes.Not
    | "And" -> CommonTypes.And
    | "Or" -> CommonTypes.Or
    | "Xor" -> CommonTypes.Xor
    | "Nand" -> CommonTypes.Nand
    | "Nor" -> CommonTypes.Nor
    | "Xnor" -> CommonTypes.Xnor
    | "Decode4" -> CommonTypes.Decode4
    | _ -> failwithf "not implemented"
    
let newSymbol (name: string) (model: Model) = 
        let sym = getStringtoComponentType name
        model, 
        (sym, "label", { X = 180. + 60. ; Y = 180. + 60.} )
        |> Symbol.Msg.AddSymbol
        |> BusWire.Msg.Symbol 
        |> Wire |> Cmd.ofMsg


let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress s ->
        match s with 
        | AltShiftZ -> 
                printStats() // print and reset the performance statistics in dev tools window
                model, Cmd.none // do nothing else and return model unchanged
        | DEL -> 
            model,
                Symbol.Msg.DeleteSymbol
                |> BusWire.Msg.Symbol
                |> Wire |> Cmd.ofMsg

        |CtrlPlus ->
            let z = model.Zoom + 0.1
            printf "zoom is %f" z
            {model with Zoom = z} , Cmd.none
        |CtrlMinus -> 
            let z = model.Zoom - 0.1
            printf "zoom is %f" z
            {model with Zoom = z} , Cmd.none

        | A -> newSymbol "Xnor" model
        | B -> newSymbol "And" model
        | C -> newSymbol "Not" model
        | D -> newSymbol "Or" model
        | E -> newSymbol "Xor" model
        | F -> newSymbol "Nand" model
        | G -> newSymbol "Nor" model
        | H -> newSymbol "Decode4" model
        | W -> printfn "asfas"
               match model.SelectedItem with
               | BusWire (wId,_) ->
                   let wModel, wCmd = BusWire.update (BusWire.AutoRouteWire wId) model.Wire
                   {model with Wire = wModel}, Cmd.map Wire wCmd
               | _ -> model, Cmd.none

        | CtrlW -> let wModel, wCmd = BusWire.update (BusWire.AutoRouteAll) model.Wire
                   {model with Wire = wModel}, Cmd.map Wire wCmd

        | R -> 
            printf "HELLO YOU HAVE PRESSED rotate"
            let selected = model.SelectedItem
            match selected with 
                | Symbol symbolID ->
                    model,
                    symbolID
                    |> Symbol.Msg.RotateSymbol
                    |> BusWire.Msg.Symbol
                    |> Wire |> Cmd.ofMsg
                | BusWire _ -> model,Cmd.none
                | NoItem -> model,Cmd.none
                | Port(portId, portType) -> failwith "Not Implemented"

        | X -> 
            printf "HELLO YOU HAVE PRESSED copy"
            model,
                    Symbol.Msg.CopySymbol
                    |> BusWire.Msg.Symbol
                    |> Wire |> Cmd.ofMsg
                    
                

        | _ -> failwithf "not yet done"

    | MouseMsg event when event.Op = MouseOp.Shift ->
        let selected = getHit event.Pos model
        match selected with
        | Symbol symbolId ->             
            {model with SelectedItem = selected}, 
                    (symbolId, event.Pos) //Zaid: added event.Pos
                    |> Symbol.Msg.MultipleSelect
                    |> BusWire.Msg.Symbol 
                    |> Wire |> Cmd.ofMsg

        | BusWire (wireId,x) -> 
            {model with SelectedItem = selected}, 
                    wireId 
                    |> BusWire.Msg.MultipleSelect |> Wire |> Cmd.ofMsg
            
        | NoItem -> 
            let sId = null 
            {model with SelectedItem = selected},
            sId |> BusWire.Msg.Select 
                      |> Wire |> Cmd.ofMsg


    | MouseMsg event when event.Op = MouseOp.Down -> 
        let selected = getHit event.Pos model 
        match selected with 
        | Symbol symbolId ->             
            {model with SelectedItem = selected}, 
            (symbolId, event.Pos)
            |> Symbol.Msg.StartDragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
            

        | BusWire (wireId,x) -> 
            {model with SelectedItem = selected}, 
            wireId 
            |> BusWire.Msg.Select |> Wire |> Cmd.ofMsg

        | NoItem -> 
            let sId = null 
            {model with SelectedItem = selected ; SelectionBox = {p1= event.Pos; p2 = event.Pos} },
            sId |> BusWire.Msg.Select 
                      |> Wire |> Cmd.ofMsg
                      
        | Port(port, portType) -> 
            {model with SelectedItem = selected}, Cmd.none

    | MouseMsg event when event.Op = MouseOp.Up -> 
       let isPortSelected = getHit event.Pos model
       match model.SelectedItem, isPortSelected with 
        | Symbol symbolId, _ ->
            model,
            Symbol.Msg.EndDragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
        | (Port (p1, type1)), (Port (p2, type2)) when type1 <> type2-> 
            {model with SelectedItem = NoItem}, 
            (if type1 = CommonTypes.PortType.Input then p1,p2 else p2,p1)
            |> BusWire.Msg.CreateWire
            |> Wire |> Cmd.ofMsg
        | NoItem, _ -> 
            printf "helloooso"
            printf "%A" model.SelectionBox
            printf "%A" event.Pos
            {model with SelectionBox = {p1= event.Pos; p2 = event.Pos} }, 
            if model.SelectionBox.p1.X > event.Pos.X && model.SelectionBox.p1.Y > event.Pos.Y then 
                (model.SelectionBox.p1,event.Pos) 
            elif model.SelectionBox.p1.X < event.Pos.X && model.SelectionBox.p1.Y > event.Pos.Y then 
                ( (posOf model.SelectionBox.p2.X model.SelectionBox.p1.Y),(posOf model.SelectionBox.p1.X model.SelectionBox.p2.Y) ) 
            elif model.SelectionBox.p1.X > event.Pos.X && model.SelectionBox.p1.Y < event.Pos.Y then
                ((posOf model.SelectionBox.p1.X model.SelectionBox.p2.Y),(posOf model.SelectionBox.p2.X model.SelectionBox.p1.Y)) 
            else 
                (event.Pos,model.SelectionBox.p1) 

            |> BusWire.Msg.SelectEnclosed
            |> Wire |> Cmd.ofMsg
        | (Port (p1, o)), _ -> 
            model, 
            BusWire.Msg.DeleteSheetWire |> Wire |> Cmd.ofMsg

        | _ -> failwithf "not yet done"

    | MouseMsg event when event.Op = MouseOp.Move ->   
        match model.SelectedItem with 
        | Port (p, pType) -> {model with SelectedItem = NoItem}, 
                              BusWire.Msg.DeleteSheetWire 
                              |> Wire |> Cmd.ofMsg
        | _ -> 
            model,
            (event.Pos, None)
            |> Symbol.Msg.MouseMove
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg

    | MouseMsg event when event.Op = Drag -> 
        match model.SelectedItem with 
        | Symbol symbolId -> 
            model,
            event.Pos 
            |> Symbol.Msg.Dragging 
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
        | BusWire (wId, segmentIndex) ->
            (if segmentIndex = 0 && (lenOfSeg (BusWire.getSegmentsFromWire (BusWire.getWireFromWireModel model.Wire wId)).Head) > portLength + 10.// ugly but works for now
             then {model with SelectedItem = BusWire (wId,2)} else model), 
            (wId, segmentIndex, event.Pos)
                |> BusWire.Msg.ManualRouting
                |> Wire
                |> Cmd.ofMsg
        | Port (p1, pType) -> 
            model,
            ( Some p1, event.Pos)
            |> BusWire.Msg.CreateSheetWire
            |> Wire |> Cmd.ofMsg
             
        | NoItem -> 
            {model with SelectionBox = {p1= model.SelectionBox.p1; p2 = event.Pos}}, 
            Cmd.none
           
       

    | MouseMsg event -> model, Cmd.none 







let init() = 
    let model,cmds = (BusWire.init)()
    let a = posOf 0.0 0.0
    {
        Wire = model
        SelectedItem = NoItem
        Zoom = 1.0
        
        SelectionBox= {p1 = a
                       p2 = a}

    }, Cmd.map Wire cmds
