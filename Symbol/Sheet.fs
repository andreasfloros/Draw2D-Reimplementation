module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open Symbol
//open CommonTypes

type SelectedItem = 
    | Sym of CommonTypes.SymbolId 

    | Wr of CommonTypes.ConnectionId * int

    | NoItem

type Model = {
    Wire: BusWire.Model
    SelectedItem: SelectedItem
    //Symbols: Symbol.Model
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | A | R

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    // | StartDragging of sId : CommonTypes.SymbolId * pagePos: XYPos
    // /// coords not adjusted for top-level zoom
    // | Dragging of sId : CommonTypes.SymbolId * pagePos: XYPos
    // | EndDragging of sId : CommonTypes.SymbolId



let getHit (point:XYPos) (model:Model) : SelectedItem = 
    printf "you are in get Hit"
    let symbolModel  =  BusWire.getSymbolModelFromWireModel model.Wire
    let hit = FindSymbol point symbolModel 
    printf "hit worked"
    match hit with 
    |Some symId -> Sym symId
    |None -> 
         let hit = BusWire.findWire point model.Wire 
         match hit with 
         |Some wr -> Wr wr 
         |None -> NoItem

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}})
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> (MouseMsg {Op = Down ; Pos = { X = ev.clientX; Y = ev.clientY }} |> dispatch))
          OnMouseUp (fun ev -> (MouseMsg {Op = Up ; Pos = { X = ev.clientX; Y = ev.clientY }} |> dispatch))
          OnMouseMove (fun ev -> if mDown ev then (MouseMsg {Op = Drag ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}} |> dispatch) else mouseOp Move ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                [ 
                    // text [ // a demo text svg element
                    //     X 500; 
                    //     Y 50; 
                    //     Style [
                    //         TextAnchor "middle" // horizontal algnment vs (X,Y)
                    //         DominantBaseline "middle" // vertical alignment vs (X,Y)
                    //         FontSize "40px"
                    //         FontWeight "Bold"
                    //         Fill "Green" // font color
                    //     ]
                    // ] [str "sample text"]

                    svgReact // the application code

                    // polygon [ // a demo svg polygon triangle written on top of the application
                    //     SVGAttr.Points "10,10 900,900 10,900"
                    //     SVGAttr.StrokeWidth "5px"
                    //     SVGAttr.Stroke "Black"
                    //     SVGAttr.FillOpacity 0.1
                    //     SVGAttr.Fill "Blue"] []
                ]
            ]
        ]




/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    |KeyPress A -> // all other keys are turned into SetColor comman
       model,(CommonTypes.Xnor, "label", {X = float (100); Y=float (100)}) |> Symbol.Msg.AddSymbol |> BusWire.Msg.Symbol |> Wire |> Cmd.ofMsg
 

    |KeyPress DEL -> // all other keys are turned into SetColor commands
        printf "HELLO YOU HAVE PRESSED delete"
        // model, Cmd.none
        let selected = model.SelectedItem
        match selected with 
            | Sym symbolID ->
                {model with SelectedItem = NoItem},
                symbolID
                // |> Symbol.Msg.DeleteSymbol
                |> BusWire.Msg.DelSym
                |> Wire |> Cmd.ofMsg
            | Wr (wId,int) -> {model with SelectedItem = selected},wId 
                              |> BusWire.Msg.DeleteWire |> Wire|>Cmd.ofMsg 
            | NoItem -> model,Cmd.none

    |KeyPress R -> // all other keys are turned into SetColor commands
        printf "HELLO YOU HAVE PRESSED rotate"
        // model, Cmd.none
        let selected = model.SelectedItem
        match selected with 
            | Sym symbolID ->
                {model with SelectedItem = NoItem},
                symbolID
                // |> Symbol.Msg.DeleteSymbol
                |> BusWire.Msg.RotSym
                |> Wire |> Cmd.ofMsg
            | Wr (wId,int) -> model,Cmd.none
            //{model with SelectedItem = selected},wId 
                              //|> BusWire.Msg.DeleteWire |> Wire|>Cmd.ofMsg 
            | NoItem -> model,Cmd.none

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress s -> // all other keys are turned into SetColor commands
        printf "HELLO YOU HAVE PRESSED s"
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

 

    | MouseMsg ev when ev.Op = Down ->
        let selected = getHit ev.Pos model 
        printf "HELLO YOU HAVE PRESSED DOWN"
        match selected with 
        | Sym symbolID ->
            printf "HELLO YOU HAVE PRESSED DOWN on Symbol"
            {model with SelectedItem = selected},
            (symbolID, ev.Pos)
            |> Symbol.Msg.StartDragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
        | Wr (wId,int) -> 
            // model, Cmd.none
            // printfn "THIS IS A WIREEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
            {model with SelectedItem = selected},wId 
            |> BusWire.Msg.Select |> Wire|>Cmd.ofMsg 
        | NoItem -> 
            let selected = model.SelectedItem
            match selected with 
            | Sym symbolID ->
                {model with SelectedItem = NoItem},
                symbolID
                |> Symbol.Msg.Unselect
                |> BusWire.Msg.Symbol
                |> Wire |> Cmd.ofMsg
            | Wr (wId,int) -> 
                // model, Cmd.none
                // printfn "THIS IS A WIREEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
                {model with SelectedItem = NoItem},wId 
                |> BusWire.Msg.Unselect |> Wire|>Cmd.ofMsg 
            | NoItem -> model,Cmd.none

    | MouseMsg ev when ev.Op = Up ->
        let selected = getHit ev.Pos model 
        match selected with 
        | Sym symbolID ->
            {model with SelectedItem = selected},
            symbolID
            |> Symbol.Msg.EndDragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
        | Wr _ -> model,Cmd.none
        | NoItem -> model,Cmd.none

    | MouseMsg ev when  ev.Op = Drag -> 
        let selected = model.SelectedItem
        match model.SelectedItem with
        | Sym symbolID ->
            model,
            (symbolID, ev.Pos)
            |> Symbol.Msg.Dragging
            |> BusWire.Msg.Symbol
            |> Wire |> Cmd.ofMsg
        | Wr (wId,int) -> 
            {model with SelectedItem = selected},(wId,int,ev.Pos) 
            |> BusWire.Msg.ManualRouting |> Wire|>Cmd.ofMsg
        | NoItem -> model,Cmd.none
    | MouseMsg ev ->
          model,Cmd.none
    | _ -> failwithf "Not implemented"

  

let init() = 
    let model,cmds = (BusWire.init)()
    {
        Wire = model
        SelectedItem = NoItem
    }, Cmd.map Wire cmds
