module Symbol

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//-------------------------------Symbol, Model, Msg definitions----------------------------//
// Symbol record contains id, position, size (assumed square), side for port (used by
// wire to figure out port placement for demo), and bounding box
type Symbol =
    {
        SymbolId : CommonTypes.ComponentId
        Pos: XYPos
        SideSize : float
        Ports : CommonTypes.Port list
        BoundingBox : BB
    }


// Model for Symbol is only a list of Symbol objects
type Model = Symbol list


// Only MoveSymbol message required for BusWire demo
type Msg =
    | MoveSymbol of sId : CommonTypes.ComponentId * clickCoords: XYPos
//-----------------------------------------------------------------------------------------//





//-------------------------------helping function definitions------------------------------//
// Returns a symbol given the symbol id
let symbolFromSymbolId (symbolId: CommonTypes.ComponentId) (model: Model) =
    model
        |> List.find (fun symbol -> symbol.SymbolId = symbolId)
                                        

// Calculates the (square) bounding box given the symbol position and size
let createBoundingBox (pos:XYPos) (size:float) =
    {
        TopLeft = pos
        BottomRight = {
                        X = pos.X + size
                        Y = pos.Y + size
                      }
    }


// Computes a basic position of the port depending on symbol parameters
// Assumes one port on each side placed on the middle of the side
let generatePortPos (symbolPos:XYPos) (size:float) (portType:CommonTypes.PortType) =
    match portType with
    | CommonTypes.PortType.Input  -> {
                                        X = symbolPos.X
                                        Y = symbolPos.Y + size / 2.
                                     }
    | CommonTypes.PortType.Output -> {
                                        X = symbolPos.X + size
                                        Y = symbolPos.Y + size / 2.
                                     }


// function to create a new symbol given appropriate parameters
let createNewSymbol (pos:XYPos) (size:float) (portType:CommonTypes.PortType) (portId: string) (portWidth: string) =
    {
        Pos = pos
        SymbolId = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
        SideSize = size
        Ports = [
                    {
                        PortId = portId
                        PortPos = generatePortPos pos size portType
                        Width = portWidth
                        PortNumber = Some 1
                        PortType = portType
                    }
                ]
        BoundingBox = createBoundingBox pos size
    }


// Used by Sheet
// Checks if a click on the canvas is on a symbol
// Accepts as inputs the click coordinates as an XYPosition as well as the model itself
// And returns either the symbol id if the click is on a symbols bounding box
// Or None if it is not (aka Option<CommonTypes.ComponentId>)
let symbolHit (clickCoords: XYPos) (model: Model) : Option<CommonTypes.ComponentId> = 
    let symbolClickedIndexOption =  model
                                    |> List.map (fun symbol -> containsPoint symbol.BoundingBox clickCoords)
                                    |> List.tryFindIndex (fun x -> x=true)
    
    if symbolClickedIndexOption=None then None    
    else
        let symbolClickedIndex = match symbolClickedIndexOption with
                                 | Some x -> x
                                 | _ -> failwithf "Doesn't happen"
        let clickedSymbol = model.[symbolClickedIndex]
        Some clickedSymbol.SymbolId
//-----------------------------------------------------------------------------------------//




//-----------------------------------Rendering functions-----------------------------------//
// No event listeners, all mouse messages come from Sheet aka only MoveSymbol
// No startDragging, Dragging, endDragging
// Component properties for rendering them
type private RenderComponentProps =
    {
        Component : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

// Actual rendering function
let private renderComponent =
    FunctionComponent.Of(
        fun (props : RenderComponentProps) ->  

            polygon
                [ 
                    let x_coord = props.Component.Pos.X
                    let y_coord = props.Component.Pos.Y
                    let sideSize = props.Component.SideSize

                    let a = string (x_coord)
                    let b = string (y_coord)
                    let c = string (x_coord + sideSize)
                    let d = string (y_coord + sideSize)


                    SVGAttr.Points (a + "," + b
                            + " " + c + "," + b
                            + " " + c + "," + d
                            + " " + a + "," + d)
                    SVGAttr.Fill "Grey"
                    SVGAttr.Stroke "Black"
                    SVGAttr.StrokeWidth 2
                ]
                [ ]
    , "Circle"
    , equalsButFunctions
    )
//-----------------------------------------------------------------------------------------//





let init () =
    [
        createNewSymbol {X=240. ; Y=470.} 60. CommonTypes.PortType.Output "1" "1"
        createNewSymbol {X=300. ; Y=260.} 80. CommonTypes.PortType.Input "2" "1"
        createNewSymbol {X=500. ; Y=200.} 70. CommonTypes.PortType.Output "3" "4"
        createNewSymbol {X=700. ; Y=700.} 100. CommonTypes.PortType.Input "4" "4"
    ]
    , Cmd.none



let view (model : Model) (dispatch : Msg -> unit) = 
    printfn "%A" model
    model
    |> List.map (fun ({SymbolId = CommonTypes.ComponentId id} as comp) ->
        renderComponent
            {
                Component = comp
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList



let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | MoveSymbol (sId,clickCoords) ->
        model
        |> List.map (fun symbol ->  if sId <> symbol.SymbolId then
                                        symbol
                                    else
                                        {symbol with
                                            Pos = clickCoords
                                            BoundingBox = createBoundingBox clickCoords symbol.SideSize
                                            Ports = symbol.Ports
                                                    |> List.map (fun port -> {port with PortPos = generatePortPos clickCoords symbol.SideSize port.PortType})
                                        }
                    ), Cmd.none