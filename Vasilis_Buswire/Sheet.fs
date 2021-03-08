module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers



// A click can select either a symbol, a wire, or nothing
// In the cases of symbol and wire, this returns their id 
// and the segment of the wire
type ClickedElement = 
    | Symbol of CommonTypes.ComponentId
    | BusWire of CommonTypes.ConnectionId * int
    | Nothing


//---------------------------------Model---------------------------------//
// For sheet the model state consists of a Wire state as well as with a 
// field of type ClickedElement which provides information as to what is
// currently selected
type Model = {
    Wire: BusWire.Model
    CurrentlySelected: ClickedElement
    }


type KeyboardMsg =
    | CtrlS | AltShiftZ | DEL | CtrlQ


type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT



let zoom = 1.0


let getHit (clickedCoords: XYPos) (model: Model) : ClickedElement  =
    let symbol = Symbol.symbolHit clickedCoords model.Wire.Symbol
    let wireSegmentOption = BusWire.wireHit clickedCoords model.Wire

    match symbol,wireSegmentOption with
    | None,      None              -> Nothing
    | Some sId , None              -> Symbol sId
    | None,      Some (wId,sIndex) -> BusWire (wId,sIndex)
    | Some sId , Some (wId,sIndex) -> Symbol sId


let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0.

    /// Dispatch a MouseMsg message for every mouse operation
    let mouseOp op (ev:Types.MouseEvent) =
        dispatch <| MouseMsg {Op=op  ;  Pos={ X = ev.clientX/zoom  ;  Y = ev.clientY/zoom }}

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
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                [ 
                    svgReact 
                ]
            ]
        ]




let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | MouseMsg mouse -> let op = mouse.Op
                        let pos = mouse.Pos

                        match op with
                            | Up   -> match model.CurrentlySelected with
                                      | Nothing     -> {model with CurrentlySelected = Nothing}, Cmd.none
                                      | Symbol  sId -> {model with CurrentlySelected = Nothing}, Cmd.none
                                      | BusWire (wid,sIndex) -> let wModel = BusWire.updateWireColor model.Wire wid
                                                                {model with CurrentlySelected = Nothing
                                                                            Wire = wModel}, Cmd.none
                            | Down -> {model with CurrentlySelected = getHit pos model}, Cmd.none
                            | Move -> model, Cmd.none
                            | Drag -> match model.CurrentlySelected with
                                      | Nothing     -> model, Cmd.none
                                      | Symbol  sId -> let wModel, wCmd = BusWire.update (BusWire.Symbol (Symbol.MoveSymbol (sId,pos))) model.Wire
                                                       {model with Wire = wModel}, Cmd.map Wire wCmd
                                      | BusWire (wid,sIndex) -> let wModel, wCmd = BusWire.update (BusWire.ManualRouting (wid,sIndex,pos)) model.Wire
                                                                {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress CtrlQ -> let wModel, wCmd = BusWire.update (BusWire.AutoRouteAll) model.Wire
                        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | _ -> failwithf "Doesn't happen"



let init() = 
    let model,cmds = (BusWire.init)()
    {
        Wire = model
        CurrentlySelected = Nothing
    }, Cmd.none
