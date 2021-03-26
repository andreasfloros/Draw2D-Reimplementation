module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

// SelectedItem allows Sheet to have information on which item is currently selected 
type SelectedItem = 
    | Symbol of symbolId: CommonTypes.SymbolId
    | BusWire of wireId: BusWire.WireId  * segmentIndex : int
    | Port of port : CommonTypes.Port * portType : CommonTypes.PortType
    | SheetSymbol of symbolId: CommonTypes.SymbolId
    | NoItem


 type Model = {
    Wire: BusWire.Model
    SelectedItem: SelectedItem
    Zoom: float
    SelectionBox: BB
    BackupModel: Model option
    }

type KeyboardMsg =  
    | CtrlS | AltShiftZ | DEL | A | B | C | D | E | F | G | H | I | CtrlW | W | R | CtrlPlus | CtrlMinus | X | CtrlZ | CtrlY

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    

let constantGridLines = gridLines 925 1100

let constantDemoMenu = 
    [                    // adds menu
        polygon 
                [
                SVGAttr.Points ("925,0 1444,0 1444,745 925,755")
                SVGAttr.StrokeWidth "2px"
                SVGAttr.Stroke "Black"
                SVGAttr.FillOpacity 0.5
                SVGAttr.Fill "lightgrey"] []

        text 
             [  SVGAttr.X 1134. ; 
                Y 60. ;
                Style [
                        FontSize "34px"
                        FontWeight "Bold"
                        Fill "Black"
                        UserSelect UserSelectOptions.None
                      ]
                 ] [str <| sprintf "Catalogue"]
    ]

//let Zoom = 1.
/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
/// 
let createBox(box : BB) =
    let p1 = box.TopLeft
    let p2 = box.BottomRight
    if p2.X< p1.X && p2.Y < p1.Y then 
        polygon [
            SVGAttr.Points $"{p2.X},{p2.Y} {p1.X},{p2.Y} {p1.X},{p1.Y} {p2.X},{p1.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeDasharray "5"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "#7a9bff"] []

    elif p2.X > p1.X && p2.Y < p1.Y then 
        polygon [
            SVGAttr.Points $"{p1.X},{p2.Y} {p2.X},{p2.Y} {p2.X},{p1.Y} {p1.X},{p1.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeDasharray "5"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "#7a9bff"] []
    elif p2.X < p1.X && p2.Y > p1.Y then 
        polygon [
            SVGAttr.Points $"{p2.X},{p1.Y} {p1.X},{p1.Y} {p1.X},{p2.Y} {p2.X},{p2.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeDasharray "5"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "#7a9bff"] []
    else 
        polygon [
            SVGAttr.Points $"{p1.X},{p1.Y} {p2.X},{p1.Y} {p2.X},{p2.Y} {p1.X},{p2.Y} "
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeDasharray "5"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "#7a9bff"] []

let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1450. * zoom))
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

    let mCtrl (ev:Types.MouseEvent) = 
        ev.ctrlKey = true

    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
            dispatch <| MouseMsg {Op = op ; Pos = { X = ev.clientX + (if container <> null then 
                                                                        container.scrollLeft - rect.left else 0.)
                                                    Y = ev.clientY + (if container <> null then container.scrollTop - rect.top else 0.)}}
    
    div [ Style 
            [ 
                // Height "100vh" 
                // MaxWidth "100vw"
                Height 745.
                MaxWidth sizeInPixels
                //CSSProp.OverflowX OverflowOptions.Auto 
                //CSSProp.OverflowY OverflowOptions.Auto
                OverflowStyle OverflowOptions.Scroll
            ]

          Id "Container"
          OnMouseDown (fun ev -> 
                        if mShift ev then (mouseOp MouseOp.Shift ev)
                        else if mCtrl ev then (mouseOp MouseOp.Ctrl ev)
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
                        Border "3px solid black"
                        Height sizeInPixels
                        Width sizeInPixels
                    ]   //ViewBox viewBoxArg
                ] // top-level transform style attribute for zoom


                (constantGridLines      // adds grid lines
                @ constantDemoMenu      // add menu
                @ [svgReact])           // add rest of canvas
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
                match Symbol.findSheetSymbol click sModel with
                | Some sId ->
                    printfn "Get hit found a sheet symbol"
                    SheetSymbol sId
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
        {model with Wire = wModel
                    BackupModel = match wMsg with 
                                  | BusWire.Msg.Symbol (Symbol.Msg.MouseMove (a,b)) -> model.BackupModel
                                  | BusWire.Msg.Symbol (Symbol.Msg.Dragging (a)) -> model.BackupModel
                                  | BusWire.Msg.Symbol (Symbol.Msg.EndDragging) -> model.BackupModel
                                  | BusWire.Msg.ManualRouting (a,b,c) -> model.BackupModel
                                  | _ -> Some model
        }, Cmd.map Wire wCmd

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
        | W -> match model.SelectedItem with
               | BusWire (wId,_) ->
                   let wModel, wCmd = BusWire.update (BusWire.AutoRouteWire wId) model.Wire
                   {model with Wire = wModel ; BackupModel = Some model}, Cmd.none
               | _ -> model, Cmd.none

        | CtrlW -> let wModel, wCmd = BusWire.update (BusWire.AutoRouteAll) model.Wire
                   {model with Wire = wModel ; BackupModel = Some model}, Cmd.none

        | R -> let selected = model.SelectedItem
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


        | X -> model,
                    Symbol.Msg.CopySymbol
                    |> BusWire.Msg.Symbol
                    |> Wire |> Cmd.ofMsg 
         
                    
        | CtrlZ | CtrlY -> let backupModel = match model.BackupModel with
                                     | Some model -> model
                                     | None -> failwithf "Doesn't happen"
                           {backupModel with BackupModel = Some model}, Cmd.none

        | _ -> model, Cmd.none


    | MouseMsg event when event.Op = MouseOp.Ctrl -> 
        let selected = getHit event.Pos model
        match selected with
        | BusWire (wireId,segmentIndex) -> 
            {model with SelectedItem = selected}, 
                    (wireId, segmentIndex, event.Pos)
                    |> BusWire.Msg.SplitSegment
                    |> Wire
                    |> Cmd.ofMsg
        | _ -> model, Cmd.none


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
            {model with SelectedItem = selected ; SelectionBox = {TopLeft= event.Pos; BottomRight = event.Pos} },
            sId |> BusWire.Msg.Select 
                      |> Wire |> Cmd.ofMsg
                      
        | Port(port, portType) -> 
            {model with SelectedItem = selected}, Cmd.none

        | SheetSymbol symbolId ->             
            {model with SelectedItem = selected}, 
            (symbolId, event.Pos)
            |> Symbol.Msg.CopySheetSymbol
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg

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
            {model with SelectionBox = {TopLeft= event.Pos; BottomRight = event.Pos} }, 
            if model.SelectionBox.TopLeft.X > event.Pos.X && model.SelectionBox.TopLeft.Y > event.Pos.Y then 
                (model.SelectionBox.TopLeft,event.Pos) 
            elif model.SelectionBox.TopLeft.X < event.Pos.X && model.SelectionBox.TopLeft.Y > event.Pos.Y then 
                ( (posOf model.SelectionBox.BottomRight.X model.SelectionBox.TopLeft.Y),(posOf model.SelectionBox.TopLeft.X model.SelectionBox.BottomRight.Y) ) 
            elif model.SelectionBox.TopLeft.X > event.Pos.X && model.SelectionBox.TopLeft.Y < event.Pos.Y then
                ((posOf model.SelectionBox.TopLeft.X model.SelectionBox.BottomRight.Y),(posOf model.SelectionBox.BottomRight.X model.SelectionBox.TopLeft.Y)) 
            else 
                (event.Pos,model.SelectionBox.TopLeft) 

            |> BusWire.Msg.SelectEnclosed
            |> Wire |> Cmd.ofMsg
        | (Port (p1, o)), _ -> 
            model, 
            BusWire.Msg.DeleteSheetWire |> Wire |> Cmd.ofMsg
        | SheetSymbol symbolId, _ ->
            model,
            Symbol.Msg.EndDragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
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
            let zeroSeg = (lenOfSeg (BusWire.getSegmentsFromWire (BusWire.getWireFromWireModel model.Wire wId)).Head) > portLength + 10.
            (if segmentIndex = 0 && zeroSeg // see manualRoute in BusWire for info on this
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
            {model with SelectionBox = {TopLeft= model.SelectionBox.TopLeft; BottomRight = event.Pos}}, 
            Cmd.none
        | SheetSymbol symbolId -> 
            model,
            event.Pos 
            |> Symbol.Msg.Dragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
    | MouseMsg event -> model, Cmd.none



let init() = 
    let model,cmds = (BusWire.init)()
    let a = posOf 0.0 0.0
    {
        Wire = model
        SelectedItem = NoItem
        Zoom = 1.0
        SelectionBox = {TopLeft = a
                        BottomRight = a}
        BackupModel = None
    }, Cmd.map Wire cmds
