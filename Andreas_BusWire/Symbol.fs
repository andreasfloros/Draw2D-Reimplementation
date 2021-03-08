module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

// Dummy Symbol implementation

// Abstract Id
type SymbolId = string

type Symbol =
    {
        SymbolPos: XYPos
        SymbolPorts : Map<CommonTypes.PortId,CommonTypes.Port>
    }

type Model = Map<SymbolId,Symbol>

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    | MoveSymbol of symbolId : SymbolId * mousePos : XYPos


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.

let generateSymbolId () = uuid()

let createSymbol (symbolPos:XYPos) (symbolPorts:Map<CommonTypes.PortId,CommonTypes.Port>)=
    {
        SymbolPos = symbolPos
        SymbolPorts = symbolPorts
    }

let createDummySymbol symbolPos inDir outDir wIn wOut=
    let portMap =
        let calculatePortPos dir =
            match dir with
            | Left -> posDiff symbolPos (posOf 20. 0.)
            | Right -> posAdd symbolPos (posOf 20. 0.)
            | Dir.Up -> posDiff symbolPos (posOf 0. 20.)
            | Dir.Down -> posAdd symbolPos (posOf 0. 20.)
        let dummyInPort = CommonTypes.createPort 0 CommonTypes.PortType.Input wIn (calculatePortPos inDir) inDir
        let dummyOutPort = CommonTypes.createPort 0 CommonTypes.PortType.Output wOut (calculatePortPos outDir) outDir
        Map[CommonTypes.generatePortId(),dummyInPort
            CommonTypes.generatePortId(),dummyOutPort]
    {SymbolPos = symbolPos; SymbolPorts = portMap}

/// Dummy function for test. The real init would probably have no symbols.
let init () = 
    let dummySymbol1 = createDummySymbol (posOf 50. 50.) Left Right 3 1
    let dummySymbolId1 = generateSymbolId()
    let dummySymbol2 = createDummySymbol (posOf 100. 100.) Dir.Up Dir.Down 3 1
    let dummySymbolId2 = generateSymbolId()
    let dummySymbol3 = createDummySymbol (posOf 250. 50.) Right Left 1 3
    let dummySymbolId3 = generateSymbolId()
    let dummySymbol4 = createDummySymbol (posOf 300. 200.) Dir.Down Dir.Up 1 1
    let dummySymbolId4 = generateSymbolId()

    let initModel = Map [(dummySymbolId1,dummySymbol1)
                         (dummySymbolId2,dummySymbol2)
                         (dummySymbolId3,dummySymbol3)
                         (dummySymbolId4,dummySymbol4)]
    initModel, Cmd.none



/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | MoveSymbol (symbolId,mousePos) ->
        let changeSymbolPosition symbolOption =
            let symbol =
                match symbolOption with
                | Some x -> x
                | _ -> failwithf "Can't happen"
            Some {
                    SymbolPos = mousePos
                    SymbolPorts = Map.map (fun _portId port ->
                                                symbol.SymbolPos
                                                |> posDiff mousePos
                                                |> CommonTypes.movePortByPos port) symbol.SymbolPorts
                 }
        let newModel = Map.change symbolId changeSymbolPosition model
        newModel, Cmd.none

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        SymbolPos : XYPos
        InDir : Dir
        OutDir : Dir
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            // dimensions count from SymbolPos
            let symbolWidth, symbolHeight = posOf 20. 0.0, posOf 0.0 20.
            let coordsInString coords =
                sprintf "%d,%d" (int coords.X) (int coords.Y)
            let bottomLeft = coordsInString <| posAdd symbolHeight (posDiff props.SymbolPos symbolWidth)
            let bottomRight = coordsInString <| posAdd symbolHeight (posAdd props.SymbolPos symbolWidth)
            let topLeft = coordsInString <| posDiff (posDiff props.SymbolPos symbolWidth) symbolHeight
            let topRight = coordsInString <| posDiff (posAdd props.SymbolPos symbolWidth) symbolHeight
            let inCoords =
                match props.InDir with
                | Left -> posDiff props.SymbolPos (posOf 10. 0.)
                | Right -> posAdd props.SymbolPos (posOf 10. 0.)
                | Dir.Up -> posDiff props.SymbolPos (posOf 0. 10.)
                | Dir.Down -> posAdd props.SymbolPos (posOf 0. 10.)
            let outCoords =
                match props.OutDir with
                | Left -> posDiff props.SymbolPos (posOf 10. 0.)
                | Right -> posAdd props.SymbolPos (posOf 10. 0.)
                | Dir.Up -> posDiff props.SymbolPos (posOf 0. 10.)
                | Dir.Down -> posAdd props.SymbolPos (posOf 0. 10.)
            g[][
                    polygon
                        [ 
                            Points (topLeft + " " + topRight + " " + bottomRight + " " + bottomLeft) 
                            SVGAttr.Fill "grey"
                            SVGAttr.Stroke "grey"
                            SVGAttr.StrokeWidth 1
                        ][]
                    text[
                            X inCoords.X
                            Y inCoords.Y
                            Style[FontSize "8px"]
                    ]["I"|>str]
                    text[
                        X (outCoords.X - 1.)
                        Y outCoords.Y
                        Style[FontSize "8px"]
                    ]["O"|>str]
                ]
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    let symbolSVG =
        model
        |> Map.map (fun _symbolId symbol->
            renderSymbol 
                {
                    SymbolPos = symbol.SymbolPos
                    InDir = Map.pick (fun id p-> if CommonTypes.getPortTypeFromPort p <> CommonTypes.Input then None else Some (CommonTypes.getDirFromPort p)) symbol.SymbolPorts
                    OutDir = Map.pick (fun id p -> if CommonTypes.getPortTypeFromPort p <> CommonTypes.Output then None else Some (CommonTypes.getDirFromPort p)) symbol.SymbolPorts
                }
        )
        |> Map.toList
        |> List.map snd
    g [] symbolSVG

let getSymbolFromId (symbolId: SymbolId) (symbolMap: Model) =
    Map.find symbolId symbolMap

let getPortsFromSymbol (symbol: Symbol) = symbol.SymbolPorts

let getSymbolPosFromSymbol (symbol: Symbol) = symbol.SymbolPos

let getPortsFromId symbolId symbolMap =
    getSymbolFromId symbolId symbolMap
    |> getPortsFromSymbol

let getSymbolBBox symbol =
    let symbolPos = getSymbolPosFromSymbol symbol
    boxOf (posAdd symbolPos (posOf 20.0 20.0)) (posDiff symbolPos (posOf 20.0 20.0))

let symbolHit mousePos symbolModel = 
    symbolModel
    |> Map.tryPick (fun symbolId symbol ->
                            let bBox = getSymbolBBox symbol
                            if isInsideBBox mousePos bBox then Some symbolId
                            else None)
