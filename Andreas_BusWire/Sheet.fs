module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type SelectedItem =
    | Symbol of symbolId: Symbol.SymbolId
    | WireSegment of wireId: BusWire.WireId * segmentIndex: int
    | NoItem

type Model = {
    WireModel: BusWire.Model
    SelectedItem: SelectedItem
    ItemProps: BusWire.WireRenderProps option // for demo only, keep the highlighted items props so that they can be reverted after deselect
    }                                         // would probably be better to include this in the SelectedItem type somehow but this will probably
                                              // all be handled by sheet so it doesn't matter here
type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT

let displaySVG (wireSVG: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" (1000.)
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| MouseMsg {Op = op ; Pos = { X = ev.clientX; Y = ev.clientY}}
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
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
                [ Style [Transform (sprintf "scale(%f)" 1.0)]] // top-level transform style attribute for zoom
                [ 
                    wireSVG // the application code

                ]
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wireSVG = BusWire.view model.WireModel (fun wireMsg -> dispatch (Wire wireMsg))
    displaySVG wireSVG dispatch
       

let getHit (mousePos: XYPos) (model: Model) : SelectedItem =

    // not optimal but works for the demo
    match Symbol.symbolHit mousePos model.WireModel.SymbolModel, BusWire.wireHit mousePos model.WireModel with
    | _, Some hit -> WireSegment hit // arbitrary priority to wires
    | Some hit, _ -> Symbol hit    
    | _ -> NoItem



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wireMsg -> 
        let newWireModel, newWireCmd = BusWire.update wireMsg model.WireModel
        {model with WireModel = newWireModel}, Cmd.map Wire newWireCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress AltC -> failwithf "Not implemented"
    | KeyPress AltV -> failwith "Not Implemented"
    | KeyPress AltZ -> model, (BusWire.Msg.AutoRouteAll |> Wire |> Cmd.ofMsg)
    | KeyPress CtrlS -> failwith "Not Implemented"
    | KeyPress DEL -> failwith "Not Implemented"
    | MouseMsg event when event.Op = Down->
        let selectedItem = getHit event.Pos model
        let newModel =
            match selectedItem with // could probably be handled better but for demoing highlighted wires this is fine, this would be handled by sheet anyway
            | WireSegment (wireId,_) -> {model with WireModel = BusWire.updateWireModelWithHighlightedWire model.WireModel wireId CommonTypes.Color.Blue
                                                    ItemProps = BusWire.getPropsFromId model.WireModel wireId |> Some}
            | _ -> model
        {newModel with SelectedItem = selectedItem}, Cmd.none
    | MouseMsg event when event.Op = Up -> 
        match model.SelectedItem with
        | WireSegment (wireId,_) ->
            let unwrappedProps = match model.ItemProps with | Some p -> p | _ -> failwithf "Shouldn't happen"
            {model with WireModel = (model.WireModel,wireId,(BusWire.getColorFromWireProps unwrappedProps))
                                    |||> BusWire.updateWireModelWithHighlightedWire
                        SelectedItem = NoItem
                        ItemProps = None}, Cmd.none
        | _ -> {model with SelectedItem = NoItem; ItemProps = None}, Cmd.none
    | MouseMsg event when event.Op = Drag ->
        match model.SelectedItem with
        | Symbol symbolId -> 
            model,
            (symbolId, event.Pos)
                |> Symbol.Msg.MoveSymbol
                |> BusWire.Msg.Symbol
                |> Wire
                |> Cmd.ofMsg
        | WireSegment (wireId, segmentIndex) ->
            (if segmentIndex = 0 && (lenOfSeg (BusWire.getSegmentsFromWire (BusWire.getWireFromWireModel model.WireModel wireId)).Head) > 11. // ugly but works for now
             then {model with SelectedItem = WireSegment (wireId,2)} else model),
            (wireId, segmentIndex, event.Pos)
                |> BusWire.Msg.ManualRouting
                |> Wire
                |> Cmd.ofMsg
        | NoItem -> model, Cmd.none
    | MouseMsg event -> model,
                        if model.SelectedItem <> NoItem then Cmd.ofMsg (MouseMsg {Op=Up;Pos=event.Pos}) // in case mouse msg up gets interrupted
                        else Cmd.none                              // final mouse message is move
    | _ -> failwithf "Not implemented"
               
let init() = 
    let initWireModel,wireCmd = (BusWire.init)()
    {WireModel = initWireModel; SelectedItem = NoItem; ItemProps = None}, Cmd.map Wire wireCmd

