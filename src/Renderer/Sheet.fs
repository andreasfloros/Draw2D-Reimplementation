module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers


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
    }

type KeyboardMsg =
    | CtrlS | AltShiftZ | DEL | A | B | C | D | E | F | G | H | I | CtrlW | W | R | ShiftA | ShiftQ

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    


//let Zoom = 1.
/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0.
    
    let mShift (ev:Types.MouseEvent) = 
        ev.shiftKey = true
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
            dispatch <| MouseMsg {Op = op ; Pos = { X = ev.clientX * zoom ; Y = ev.clientY * zoom}}


    div [ Style 
            [ 
                // Height "100vh" 
                // MaxWidth "100vw"
                Height sizeInPixels
                MaxWidth sizeInPixels
                //CSSProp.OverflowX OverflowOptions.Auto 
               //CSSProp.OverflowY OverflowOptions.Auto
            ] 

          OnMouseDown (fun ev -> 
                        if mShift ev
                        then (MouseMsg {Op = MouseOp.Shift; Pos = {X = ev.clientX ; Y = ev.clientY}} |> dispatch)
                        else (MouseMsg {Op = MouseOp.Down; Pos = {X = ev.clientX ; Y = ev.clientY}} |> dispatch))

         

          OnMouseUp (fun ev -> (MouseMsg {Op = MouseOp.Up; Pos = {X = ev.clientX ; Y = ev.clientY}} |> dispatch))

          OnMouseMove (fun ev -> 
                        if mDown ev 
                        then (MouseMsg {Op = Drag; Pos = {X = ev.clientX ; Y = ev.clientY}} |> dispatch)
                        else mouseOp Move ev)
                    
    ]
        
        [ svg
            [ Style [
                Transform (sprintf "scale(%f)" zoom)
                Border "3px solid blue"
                Height sizeInPixels
                Width sizeInPixels    
                ]] // top-level transform style attribute for zoom

                [svgReact] // the application code
        ]


/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model.Zoom wireSvg dispatch


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

        |ShiftA ->
            let z = model.Zoom + 0.1
            printf "zoom is %f" z
            {model with Zoom = z} , Cmd.none
        |ShiftQ -> 
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
        | W -> failwithf "Not yet implemented"

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
        let newModel = {model with SelectedItem = NoItem}
        match selected with 
        | Symbol symbolId ->             
            if selected <> model.SelectedItem then 
                {model with SelectedItem = selected}, 
                (symbolId, event.Pos)
                |> Symbol.Msg.StartDragging
                |> BusWire.Msg.Symbol
                |> Wire |> Cmd.ofMsg
            else 
                {model with SelectedItem = NoItem}, 
                symbolId 
                |> Symbol.Msg.Unselect
                |> BusWire.Msg.Symbol 
                |> Wire |> Cmd.ofMsg

        | BusWire (wireId,x) -> 
            if selected <> model.SelectedItem then
                {model with SelectedItem = selected}, 
                wireId 
                |> BusWire.Msg.Select |> Wire |> Cmd.ofMsg
            else 
                newModel, 
                wireId 
                |> BusWire.Msg.Deselect 
                |> Wire |> Cmd.ofMsg

        | NoItem -> 
            let sId = null 
            {model with SelectedItem = selected},
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
            model, 
            Cmd.none
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
        | BusWire (wId, x) ->
            {model with SelectedItem = BusWire (wId,x)}, 
            (wId, x, event.Pos)
                |> BusWire.Msg.ManualRouting
                |> Wire
                |> Cmd.ofMsg
        | Port (p1, pType) -> 
            model,
            ( Some p1, event.Pos)
            |> BusWire.Msg.CreateSheetWire
            |> Wire |> Cmd.ofMsg
             
        | NoItem -> 
            let s = model.SelectedItem
            let newModel = {model with SelectedItem = NoItem}
            match s with 
            | Symbol sId -> 
                    newModel, 
                    sId |> Symbol.Msg.Unselect 
                    |> BusWire.Msg.Symbol
                    |> Wire |> Cmd.ofMsg
            | BusWire (wId,x) -> 
                        newModel, 
                        wId |> BusWire.Msg.Deselect
                        |> Wire |> Cmd.ofMsg
            | NoItem -> newModel, Cmd.none
            | Port(portId, portType) -> failwith "Not Implemented"
        | _ -> failwithf "not yet done"

    | MouseMsg event -> model, Cmd.none 





let init() = 
    let model,cmds = (BusWire.init)()
    {
        Wire = model
        SelectedItem = NoItem
        Zoom = 1.0

    }, Cmd.map Wire cmds
