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
type Symbol = CommonTypes.Component

type Model = {
    SymModel: Symbol list
    SheetSymbol: Symbol list
}
//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.SymbolId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of pagePos: XYPos
    | EndDragging 
    | Unselect of sId : CommonTypes.SymbolId 
    | AddSymbol of CompType: CommonTypes.ComponentType * label: string * pagePos: XYPos 
    | CopySymbol
    | DeleteSymbol 
    | RotateSymbol of sId:CommonTypes.SymbolId 
    | MultipleSelect of sId : CommonTypes.SymbolId * pagePos: XYPos
    | MouseMove of pagePos : XYPos * PortSelect: Port Option
    | Deselect
    | SelectEnclosed of p1: XYPos * p2: XYPos
    | CopySheetSymbol of sId: CommonTypes.SymbolId * pagePos: XYPos


//---------------------------------helper types and functions----------------//


let createBB (sym: Symbol) (h,w: float) =
    {
        TopLeft = {X =  sym.Pos.X ; Y = sym.Pos.Y}
        BottomRight = {X =  sym.Pos.X + w ; Y = sym.Pos.Y + h}
    }

let FindSymbol (mousePos: XYPos) (model: Model) = 
    match List.tryFind (fun sym -> containsPoint (createBB sym (sym.CurrentH,sym.CurrentW)) mousePos) (model.SymModel) with 
    | Some sym -> Some sym.Id
    | None -> None

let findSheetSymbol (mousePos: XYPos) (model: Model) = 
    match List.tryFind (fun sym -> containsPoint (createBB sym (sym.CurrentH,sym.CurrentW)) mousePos) (model.SheetSymbol) with 
    | Some sym -> Some sym.Id
    | None -> None

let getSelectedSymbols (model: Model) = 
    model.SymModel |> List.filter (fun sym -> (sym.IsSelected = true))
                      |> List.map (fun s -> s.Id)


let IsNoSymbolSelected model = List.isEmpty (getSelectedSymbols model)
    

let createBBMouseHover (sym: Symbol) (h,w: float) =
    {
        TopLeft = {X =  sym.Pos.X - 50.; Y = sym.Pos.Y - 50.}
        BottomRight = {X =  sym.Pos.X + w + 50.; Y = sym.Pos.Y + h + 50.}
    }


let distFromBB (symPos: XYPos)  (h,w: float)  (mPos : XYPos)  : float=
    
    if mPos.X < symPos.X  then  (abs(  symPos.X - mPos.X - 50.)) / 50.
   
    else match  mPos.X > (symPos.X + w) with
         | true -> (abs(mPos.X - symPos.X - w - 50.))/50.
         | false -> if mPos.Y > (symPos.Y + h)
                    then (abs(mPos.Y - symPos.Y - h - 50.))/50.
                    else  (abs(symPos.Y - mPos.Y - 50.))/50.




//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//


type CompProps = {
        Id : SymbolId
        Pos : XYPos
        Type : ComponentType
        NumOfInputs : int
        NumOfUpwardInputs : int
        H : float
        W : float
        HeaderMargin : float
        FooterMargin : float
    }


let generatePortList compProps numOfPorts portType connectionDirection =
    
    let busWidth portNum = 
        match compProps.Type, portType, portNum with
        | Decode4, PortType.Input, 0 -> Some 2

        | NbitsAdder _, PortType.Input, 0 -> Some 1
        | NbitsAdder bw, PortType.Input, _ -> Some bw
        | NbitsAdder bw, PortType.Output, 0 -> Some bw
        | NbitsAdder _, PortType.Output, _ -> Some 1
        
        | Mux4, PortType.Input, 4 | Demux4, PortType.Output, 1 -> Some 2
        | MuxN n, PortType.Input, i when i = n -> Some (log2 n)
        | DemuxN n, PortType.Input, 1 -> Some (log2 n)

        | Input bw, _, _ | Output bw, _, _ -> Some bw
        | IOLabel, _, _ -> Some 1 //placeholder (should be dynamically allocated using width inference)

        | OldBusSelection _, PortType.Input, _ | BusSelection _, PortType.Input, _ -> Some 10 //placeholder (should be dynamically allocated using width inference)
        | OldBusSelection (bw,_), PortType.Output, _ | BusSelection (bw,_), PortType.Output, _ -> Some bw

        | MergeWires, _, _ -> Some 1 //placeholder (should be dynamically allocated using width inference)
        | SplitWire bw, PortType.Output, 0 -> Some bw
        | SplitWire _, _, _ -> Some 1 //placeholder (should be dynamically allocated using width inference)

        | Register _, PortType.Input, 1 | RegisterE _, PortType.Input, 1 -> Some 1
        | Register bw, _, _ | RegisterE bw, _, _  -> Some bw

        | AsyncROM memory, PortType.Input, _ | ROM memory, PortType.Input, _ | RAM memory, PortType.Input, 0 -> Some memory.AddressWidth
        | AsyncROM memory, PortType.Output, _ | ROM memory, PortType.Output, _  
        | RAM memory, PortType.Input, 1 | RAM memory, PortType.Output, _ -> Some memory.WordWidth
        | RAM memory, PortType.Input, _ -> Some 1

        | Custom features, PortType.Input, _ -> Some (snd features.InputLabels.[portNum])
        | Custom features, PortType.Output, _ -> Some (snd features.OutputLabels.[portNum])

        | _ -> Some 1

    match portType,connectionDirection with
    
    | PortType.Input, Up -> 
        let portDists = compProps.W/float(numOfPorts+1)
        let relativePortPos i = 
            match compProps.Type with
            | Mux2 | Mux4 | MuxN _ -> {X = float(i)*portDists ; Y = compProps.H - float(i*15)}
            | Demux2 | Demux4 | DemuxN _ -> {X = float(i)*portDists ; Y = compProps.H - compProps.FooterMargin + float(i*15)}
            | _ -> {X = float(i)*portDists ; Y = compProps.H}
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (i - 1 + compProps.NumOfInputs - compProps.NumOfUpwardInputs)
                PortType = portType
                PortPos = {X = compProps.Pos.X + (relativePortPos i).X ; Y = compProps.Pos.Y + (relativePortPos i).Y}
                RelativePortPos = relativePortPos i
                BusWidth = busWidth (i-1)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })

    | PortType.Input, Down -> 
        let portDists = compProps.W/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (i - 1 + compProps.NumOfInputs - compProps.NumOfUpwardInputs)
                PortType = portType
                PortPos = {X = compProps.Pos.X + float(i)*portDists ; Y = compProps.Pos.Y}
                RelativePortPos = {X = float(i)*portDists ; Y = 0.}
                BusWidth = busWidth (i-1)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })

    | PortType.Input, _ -> 
        let portDists = (compProps.H - compProps.HeaderMargin - compProps.FooterMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (i-1)
                PortType = portType
                PortPos = {X = compProps.Pos.X ; Y = compProps.Pos.Y + compProps.HeaderMargin + (float(i)*portDists)}
                RelativePortPos = {X = 0. ; Y = compProps.HeaderMargin + (float(i)*portDists)}
                BusWidth = busWidth (i-1)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })

    | PortType.Output, _ -> 
        let portDists = (compProps.H - compProps.HeaderMargin - compProps.FooterMargin)/float(numOfPorts+1)
        [1..numOfPorts] 
        |> List.map (fun i -> 
            {
                Id = Helpers.uuid()
                PortNumber = Some (i-1)
                PortType = portType
                PortPos = {X = compProps.Pos.X + compProps.W ; Y = compProps.Pos.Y + compProps.HeaderMargin + (float(i)*portDists)}
                RelativePortPos = {X = compProps.W ; Y = compProps.HeaderMargin + (float(i)*portDists)}
                BusWidth = busWidth (i-1)
                ConnectionDirection = connectionDirection
                HostId = compProps.Id
            })

/// Used to calculate the properties of any 'basic' component that does not contain 
/// any pins at the bottom of the symbol.
// To add new compnonets: add a line to the top match statement and provide the number 
// of input pins and the number of output pins. Later match statements can be used to 
// implement unique symbol design.
let generateBasicComp compType compId compPos =
    let ((numOfInputs:int), (numOfOutputs:int)) =
        match compType with 
        | Not | OldBusSelection _ | BusSelection _ | DFF | Register _ | IOLabel | AsyncROM _ | ROM _ -> (1, 1)
        | And | Or | Xor | Nand | Nor | Xnor | MergeWires -> (2, 1)
        | Decode4 -> (2, 4)
        | NbitsAdder _ -> (3, 2)
        | Input _ | Constant _ -> (0, 1)
        | Output _ -> (1, 0)
        | SplitWire _ -> (1, 2)
        | RAM _ -> (3, 1)
        | Custom features -> ((List.length features.InputLabels), (List.length features.OutputLabels))
        | _ -> failwithf "Error: Component I/O not specified"

    /// A vertical extension at the top of the symbol that Left/Right Ports 
    /// do not have access to.
    let headerMargin = 
        match compType with 
        | Decode4 | NbitsAdder _ | DFF | Register _ | AsyncROM _ | ROM _ | RAM _ | Custom _ -> 20.
        | MergeWires | SplitWire _ -> -30.
        | _ -> 0.

    /// A vertical extension at the bottom of the symbol that Left/Right Ports 
    /// do not have access to. 
    let footerMargin =
        match compType with 
        | DFF | Register _ | AsyncROM _ | ROM _ | RAM _ -> 15.
        | MergeWires | SplitWire _ -> -30.
        | _ -> 0.

    let height = 
        match compType with 
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 50.
        | Input _ | Output _ | IOLabel -> 25.
        | Constant _ -> 20.
        | OldBusSelection _ | BusSelection _ | MergeWires | SplitWire _ -> 30.
        | Register _ -> 90.
        | AsyncROM _ | ROM _ | RAM _ -> 130.
        | _ ->  headerMargin + footerMargin + 20. + float(20 * List.max [numOfInputs; numOfOutputs])

    let width = 
        match compType with 
        | Not | Nand | Nor | Xnor | DFF -> 60.
        | Input _ | Output _ | IOLabel | Constant _ -> 35.
        | Decode4 | NbitsAdder _ -> 80.
        | Register _ | AsyncROM _ | ROM _ | RAM _ | Custom _ -> 130.
        | _ -> 50.  
    
    let compProps = {
        Id = compId
        Pos = compPos
        Type = compType
        NumOfInputs = numOfInputs
        NumOfUpwardInputs = 0
        H = height
        W = width
        HeaderMargin = headerMargin
        FooterMargin = footerMargin
    }

    let inPortList = generatePortList compProps numOfInputs PortType.Input Right 
    let outPortList = generatePortList compProps numOfOutputs PortType.Output Left  
    ((List.append inPortList outPortList), compProps.H, compProps.W, numOfInputs, numOfOutputs, 0)


/// Used to calculate the properties of any component that contains inputs that connect  
/// to the bottom of the symbol (typically select pins). Eg: Multiplexers.
/// To add new compnonets: add a line to the match statement and provide the number 
/// of right-connecting input pins, the number of upward-connecting input pins and 
/// the number of output pins.
let generateCompWithUpwardInputs compType compId compPos =
    let ((numOfRightInputs:int), (numOfUpwardInputs:int), (numOfOutputs:int)) =
        match compType with 
        | Mux2 -> (2, 1, 1)
        | Demux2 -> (1, 1, 2)
        | Mux4 -> (4, 1, 1)
        | Demux4 -> (1, 1, 4)
        | MuxN n -> (n, 1, 1)
        | DemuxN n -> (1, 1, n)
        | DFFE | RegisterE _ -> (1, 1, 1)
        | _ -> failwithf "Error: Component I/O not repcified"

    /// A vertical extension at the top of the symbol that Left/Right Ports 
    /// do not have access to.
    let headerMargin = 
        match compType with 
        | Mux2 | Demux2 | Mux4 | Demux4 | MuxN _ | DemuxN _ -> 40. //20. + float(numOfUpwardInputs*10)
        | DFFE | RegisterE _ -> 20.
        | _ -> 0.

    /// A vertical extension at the bottom of the symbol that Left/Right Ports 
    /// do not have access to. 
    let footerMargin = 
        match compType with 
        | Mux2 | Demux2 | Mux4 | Demux4 | MuxN _ | DemuxN _ -> 30. //10. + float(numOfUpwardInputs*10)
        | DFFE | RegisterE _ -> 15.
        | _ -> 0.

    let height = 
        match compType with 
        | RegisterE _ -> 90.
        | _ -> headerMargin + footerMargin + 20. + float(20 * List.max [numOfRightInputs; numOfOutputs])

    let width = 
        match compType with 
        | Mux2 | Demux2 | Mux4 | Demux4 | MuxN _ | DemuxN _ -> 70. //25. + float(25*numOfUpwardInputs)
        | DFFE -> 100.
        | RegisterE _ -> 130.
        | _ -> 50.

    let compProps = {
            Id = compId
            Pos = compPos
            Type = compType
            NumOfInputs = numOfRightInputs + numOfUpwardInputs
            NumOfUpwardInputs = numOfUpwardInputs
            H = height
            W = width
            HeaderMargin = headerMargin
            FooterMargin = footerMargin
        }

    let inPortList = generatePortList compProps numOfRightInputs PortType.Input Dir.Right 
    let selectPortList = generatePortList compProps numOfUpwardInputs PortType.Input Dir.Up 
    let outPortList = generatePortList compProps numOfOutputs PortType.Output Dir.Left   
    ((inPortList @ selectPortList @ outPortList), compProps.H, compProps.W, (numOfRightInputs+numOfUpwardInputs), numOfOutputs, numOfUpwardInputs)
    

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (compType:CommonTypes.ComponentType) (label:string) (pos:XYPos) : CommonTypes.Component = 
    let compId = SymbolId (Helpers.uuid())
    let (portList, height, width, numOfInputs, numOfOutputs, numOfUpwardInputs) = 
        match compType with

        | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | NbitsAdder _ 
        | OldBusSelection _ | BusSelection _ | DFF | Register _ | Custom _
        | MergeWires | SplitWire _ | Input _ | Output _ | IOLabel | Constant _ 
        | AsyncROM _ | ROM _ | RAM _ -> generateBasicComp compType compId pos

        | Mux2 | Demux2 | Mux4 | Demux4 | MuxN _ | DemuxN _ 
        | DFFE | RegisterE _ -> generateCompWithUpwardInputs compType compId pos

        | _ -> failwithf "Error: component not implemented"
    {
        Id = compId
        Type = compType
        Label = label 
        Ports = portList
        NumOfInputs = numOfInputs
        NumOfOutputs = numOfOutputs
        NumOfUpwardInputs = numOfUpwardInputs
        Pos = pos
        H = height
        W = width
        CurrentH = height
        CurrentW = width
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false 
        IsSelected = false
        Orientation = Standard
        MouseNear = (posOf 0.0 0.0),None
    } 


let createSymbolCopy (sym:Symbol) isSheetSymbol mousePos=
    let symCopyId = SymbolId (Helpers.uuid())
    let symCopyPos = if isSheetSymbol then sym.Pos else posAdd sym.Pos {X = 20.;Y = 20.}
    {
        Id = symCopyId
        Type = sym.Type
        Label = sym.Label + "_Copy" 
        Ports = List.map (fun port -> 
            {port with 
                Id = Helpers.uuid()
                PortPos = posAdd symCopyPos port.RelativePortPos
                HostId = symCopyId
            }) sym.Ports 
        NumOfInputs = sym.NumOfInputs
        NumOfOutputs = sym.NumOfOutputs
        NumOfUpwardInputs = sym.NumOfUpwardInputs
        Pos = symCopyPos
        H = sym.H
        W = sym.W
        CurrentH = sym.CurrentH
        CurrentW = sym.CurrentW
        LastDragPos = if isSheetSymbol then mousePos else {X=0. ; Y=0.} 
        IsDragging = isSheetSymbol 
        IsSelected = isSheetSymbol
        Orientation = sym.Orientation
        MouseNear = (posOf 0.0 0.0),None
    } 


let testAsyncROM = AsyncROM {AddressWidth = 4; WordWidth = 2; Data = Map.ofList [(int64(5),int64(2))]}
let testROM = ROM {AddressWidth = 4; WordWidth = 2; Data = Map.ofList [(int64(5),int64(2))]}
let testRAM = RAM {AddressWidth = 4; WordWidth = 2; Data = Map.ofList [(int64(5),int64(2))]}
let testCustom = Custom {Name = "Custom Comp Test"; InputLabels = [("DATA",4);("R/W",1)]; OutputLabels = [("out_1",2);("out_2",2);("out_3",2)] }


let init () =
    
    {   SheetSymbol = [createNewSymbol (Input 4) "Input Example" {X=925.+125.;Y=130.}
                       createNewSymbol (Output 4) "Output Example" {X=925.+415.;Y=130.}
                       createNewSymbol (And) "And Example" {X=925.+125.;Y=230.}
                       createNewSymbol (Or) "Or Example" {X=925.+400.;Y=230.}
                       createNewSymbol (Not) "Not Example" {X=925.+260.;Y=230.}
                       createNewSymbol (MuxN 4) "MuxN Example" {X=925.+125.;Y=370.}
                       createNewSymbol (NbitsAdder 10) "NbitAdder Example" {X=925.+250.;Y=400.}
                       createNewSymbol (DemuxN 4) "DemuxN Example" {X=925.+385.;Y=370.}
                      ]



        SymModel = List.allPairs [1..4] [1..2]
                |> List.map (fun (x,y) -> {X = float (x*180+20); Y=float (y*220-60)})
                |> List.map (fun {X=x;Y=y} -> 
                    match (x, y) with 
                    | 200., 160. -> (createNewSymbol (Input 7) "Input Example" {X=x;Y=y})
                    | 200., 380. -> (createNewSymbol (NbitsAdder 10) "label" {X=x;Y=y})
                    | 380., 160. -> (createNewSymbol (BusSelection (5,2)) "BusSelect1" {X=x;Y=y})
                    | 380., 380. -> (createNewSymbol (testRAM) "label" {X=x;Y=y})
                    | 560., 160. -> (createNewSymbol (DemuxN 7) "label" {X=x;Y=y})
                    | 560., 380. -> (createNewSymbol (testCustom) "label" {X=x;Y=y})
                    | 740., 160. -> (createNewSymbol (Output 5) "label" {X=x;Y=y})
                    | 740., 380. -> (createNewSymbol (MuxN 4) "label" {X=x;Y=y})
                    | _ -> (createNewSymbol (Xor) "label" {X=x;Y=y}))
    }
        
    , Cmd.none


/// Alters Ports of a symbol upon rotation
let portRotation sym port : Port =
    let prevRelativePos = port.RelativePortPos
    let prevDir = port.ConnectionDirection

    match sym.Orientation with

    | Standard ->
        let relativePortPos = {X = sym.H-prevRelativePos.Y ; Y = prevRelativePos.X}
        {port with
            PortPos = {X = sym.Pos.X + relativePortPos.X ; Y = sym.Pos.Y + relativePortPos.Y}
            RelativePortPos = relativePortPos
            ConnectionDirection = 
                match prevDir with
                | Left -> Up            
                | Up -> Right
                | Right -> Down
                | Down -> Left
        }

    | Rotate90clk ->
        let relativePortPos = {X = sym.W-(prevRelativePos.Y) ; Y = sym.H-prevRelativePos.X} 
        {port with
            PortPos = {X = sym.Pos.X + relativePortPos.X ; Y = sym.Pos.Y + relativePortPos.Y}
            RelativePortPos = relativePortPos
            ConnectionDirection = 
                match prevDir with
                | Left -> Down            
                | Up -> Right
                | Right -> Up
                | Down -> Left
        }

    | Mirror ->
        let relativePortPos = {X = prevRelativePos.Y ; Y = prevRelativePos.X} 
        {port with
            PortPos = {X = sym.Pos.X + relativePortPos.X ; Y = sym.Pos.Y + relativePortPos.Y}
            RelativePortPos = relativePortPos
            ConnectionDirection = 
                match prevDir with
                | Left -> Up            
                | Up -> Left
                | Right -> Down
                | Down -> Right
        }

    | Rotate90antiClk ->
        let relativePortPos = {X = sym.W - prevRelativePos.Y ; Y = prevRelativePos.X} 
        {port with
            PortPos = {X = sym.Pos.X + relativePortPos.X ; Y = sym.Pos.Y + relativePortPos.Y}
            RelativePortPos = relativePortPos
            ConnectionDirection = 
                match prevDir with
                | Left -> Up            
                | Up -> Right
                | Right -> Down
                | Down -> Left
        }

/// Changes symbol properties according to rotation:
let symbolRotation sym = 
    let prevPorts = sym.Ports

    match sym.Type with 
    | Input _ | Output _ | IOLabel -> sym
    | _ ->
        match sym.Orientation with
        | Standard ->
            {sym with
                Ports = List.map (portRotation sym) prevPorts
                Orientation = Rotate90clk
                CurrentH = sym.W
                CurrentW = sym.H
            }
        | Rotate90clk ->
            {sym with
                Ports = List.map (portRotation sym) prevPorts
                Orientation = Mirror
                CurrentH = sym.H
                CurrentW = sym.W
            }
        | Mirror ->
            {sym with
                Ports = List.map (portRotation sym) prevPorts
                Orientation = Rotate90antiClk
                CurrentH = sym.W
                CurrentW = sym.H
            }
        | Rotate90antiClk ->
            {sym with
                Ports = List.map (portRotation sym) prevPorts
                Orientation = Standard
                CurrentH = sym.H
                CurrentW = sym.W
            }

/// Checks whether two values are close together
let closeTogether a b = 
    match a, b with 
    | v1, v2 when abs(v1 - v2) <= 10. -> true // Set "closeness" threshold here
    | _ -> false

/// Checks whether a given symbol horizontally aligns with an other symbol in the model
/// (Sides align with sides and centres with centres)
let horizontalAlign (sym:Symbol) xPos model =
    let checkSideAlignment (w:float) (s:Symbol) =
        let x = xPos + w
        if sym.Id <> s.Id 
        then
            match (closeTogether x s.Pos.X), (closeTogether x (s.Pos.X + s.CurrentW)) with 
            | false, false -> None
            | true, false -> Some s.Pos.X
            | false, true -> Some (s.Pos.X + s.CurrentW)
            | true, true when abs(x - s.Pos.X) <= abs(x - (s.Pos.X + s.CurrentW))-> Some s.Pos.X
            | true, true -> Some (s.Pos.X + s.CurrentW)
        else 
            None
    let allSideComparisons w = List.map (checkSideAlignment w) model

    let checkCentreAlignment (s:Symbol) =
        let x = xPos + (sym.CurrentW/2.)
        if sym.Id <> s.Id 
        then
            match (closeTogether x (s.Pos.X + (s.CurrentW/2.))) with 
            | false -> None
            | true -> Some (s.Pos.X + (s.CurrentW/2.))
        else 
            None

    let keepClosest w a b =
        let x = xPos + w
        match a, b with
        | None, None -> None 
        | None, Some q -> Some q
        | Some p, None -> Some p
        | Some p, Some q when abs(x - p) <= abs(x - q) -> Some p
        | Some p, Some q -> Some q

    let leftSide = (None, (allSideComparisons 0.)) ||> List.fold (keepClosest 0.)
    let rightSide = (None, (allSideComparisons sym.CurrentW)) ||> List.fold (keepClosest sym.CurrentW)
    let centre = (None, (List.map checkCentreAlignment model)) ||> List.fold (keepClosest (sym.CurrentW/2.))

    // Checks which of the three alignments takes priority
    match leftSide, rightSide, centre with
    | None, None, None -> None
    | Some l, None, None -> Some l
    | Some l, Some r, None when abs(xPos - l) <= abs(xPos + sym.CurrentW - r) -> Some l
    | _, Some r, None -> Some (r - sym.CurrentW)
    | Some l, None, Some c when abs(xPos - l) <= abs(xPos + (sym.CurrentW/2.) - c) -> Some l
    | Some l, None, Some c -> Some (c - (sym.CurrentW/2.))
    | Some l, Some r, Some c when (abs(xPos - l) <= abs(xPos + sym.CurrentW - r)) && (abs(xPos - l) <= abs(xPos + (sym.CurrentW/2.) - c)) -> Some l
    | _, Some r, Some c when abs(xPos + sym.CurrentW - r) <= abs(xPos + (sym.CurrentW/2.) - c) -> Some (r - sym.CurrentW)
    | _, _, Some c -> Some (c - (sym.CurrentW/2.))

/// Checks whether a given symbol vertically aligns with an other symbol in the model
/// (Sides align with sides and centres with centres)
let verticalAlign (sym:Symbol) yPos model =
    let checkSideAlignment h (s:Symbol) =
        let y = yPos + h
        if sym.Id <> s.Id 
        then
            match (closeTogether y s.Pos.Y), (closeTogether y (s.Pos.Y + s.CurrentH)) with 
            | false, false -> None
            | true, false -> Some s.Pos.Y
            | false, true -> Some (s.Pos.Y + s.CurrentH)
            | true, true when abs(y - s.Pos.Y) <= abs(y - (s.Pos.Y + s.CurrentH))-> Some s.Pos.Y
            | true, true -> Some (s.Pos.Y + s.CurrentH)
        else 
            None
    let allSideComparisons h = List.map (checkSideAlignment h) model

    let checkCentreAlignment (s:Symbol) =
        let y = yPos + (sym.CurrentH/2.)
        if sym.Id <> s.Id 
        then
            match (closeTogether y (s.Pos.Y + (s.CurrentH/2.))) with 
            | false -> None
            | true -> Some (s.Pos.Y + (s.CurrentH/2.))
        else 
            None

    let keepClosest h a b =
        let y = yPos + h
        match a, b with
        | None, None -> None 
        | None, Some q -> Some q
        | Some p, None -> Some p
        | Some p, Some q when abs(y - p) <= abs(y - q) -> Some p
        | Some p, Some q -> Some q

    let topSide = (None, (allSideComparisons 0.)) ||> List.fold (keepClosest 0.)
    let bottomSide = (None, (allSideComparisons sym.CurrentH)) ||> List.fold (keepClosest sym.CurrentH)
    let centre = (None, (List.map checkCentreAlignment model)) ||> List.fold (keepClosest (sym.CurrentH/2.))

    // Checks which of the three alignments takes priority
    match topSide, bottomSide, centre with
    | None, None, None -> None
    | Some t, None, None -> Some t
    | Some t, Some b, None when abs(yPos - t) <= abs(yPos + sym.CurrentH - b) -> Some t
    | _, Some b, None -> Some (b - sym.CurrentH)
    | Some t, None, Some c when abs(yPos - t) <= abs(yPos + (sym.CurrentH/2.) - c) -> Some t
    | Some t, None, Some c -> Some (c - (sym.CurrentH/2.))
    | Some t, Some b, Some c when (abs(yPos - t) <= abs(yPos + sym.CurrentH - b)) && (abs(yPos - t) <= abs(yPos + (sym.CurrentH/2.) - c)) -> Some t
    | _, Some b, Some c when abs(yPos + sym.CurrentH - b) <= abs(yPos + (sym.CurrentH/2.) - c) -> Some (b - sym.CurrentH)
    | _, _, Some c -> Some (c - (sym.CurrentH/2.))


/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (compType, label, pos) -> 
        {model with SymModel = (createNewSymbol compType label pos) :: (model.SymModel)}, Cmd.none

    | CopySymbol ->
        {model with SymModel = 
                    (List.collect (fun sym ->
                    if sym.IsSelected = true then
                        [createSymbolCopy sym false (posOf 0. 0.)]
                    else
                        []
                    ) model.SymModel) 
                    @ (List.map (fun sym ->
                        if sym.IsSelected = true then
                            {sym with 
                               IsSelected = false 
                            }
                        else sym
                    ) model.SymModel)}
        , Cmd.none
    
    | CopySheetSymbol (sId,mousePos) ->
        let symbolToBeAdded = List.find (fun (sym: Symbol)  -> sym.Id = sId) model.SheetSymbol
        let copy = createSymbolCopy symbolToBeAdded true mousePos
        let newSymbolList = model.SymModel @ [copy]
                            |> List.map (fun sym -> if sym.Id = copy.Id then sym else {sym with IsSelected = false})
        {model with SymModel = newSymbolList}, Cmd.ofMsg (Dragging mousePos)
        

    | DeleteSymbol -> 
    {model with SymModel = List.filter (fun sym -> sym.IsSelected = false) model.SymModel}, Cmd.none

    | RotateSymbol sId ->
        {model with SymModel = model.SymModel
                    |> List.map (fun sym ->
                        if sId <> sym.Id 
                        then sym
                        else symbolRotation sym
                    )}
        , Cmd.none

    | StartDragging (sId, pagePos) ->
        {model with SymModel = model.SymModel
                |> List.map (fun sym ->
                    if sId <> sym.Id then
                        {sym with 
                           IsSelected = false 
                        }

                    else
                        { sym with
                            LastDragPos = pagePos
                            IsDragging = true
                            IsSelected = true 
                        }
                )}
        , Cmd.none
    | Deselect ->
        {model with 
            SymModel = model.SymModel 
            |> List.map (fun sym -> {sym with IsSelected = false})}, Cmd.none

    | Dragging pagePos ->
        {model with
            SymModel = model.SymModel
            |> List.map (fun sym ->
                if sym.IsSelected = false then
                    sym
                else
                    let diff = posDiff pagePos sym.LastDragPos
                    let updatedPos = posAdd sym.Pos diff
                    let newX, xAligned = 
                        match horizontalAlign sym updatedPos.X model.SymModel with
                        | None -> updatedPos.X, false
                        | Some x -> x, true
                    let newY, yAligned = 
                        match verticalAlign sym updatedPos.Y model.SymModel with
                        | None -> updatedPos.Y, false
                        | Some y -> y, true
                    { sym with
                        Pos = updatedPos //{X = newX; Y = newY} //updatedPos //
                        Ports = List.map (fun port -> {port with PortPos = posAdd port.RelativePortPos sym.Pos}) sym.Ports
                        LastDragPos = pagePos
                            // if ((xAligned = true) || (yAligned = true))
                            // then sym.LastDragPos
                            // else pagePos
                    }
            )}
        , Cmd.none

    | EndDragging ->
        {model with
            SymModel = model.SymModel
            |> List.map (fun sym ->
                if sym.IsSelected = false then 
                    sym
                else
                    let newX = 
                        match horizontalAlign sym sym.Pos.X model.SymModel with
                        | None -> sym.Pos.X
                        | Some x -> x
                    let newY = 
                        match verticalAlign sym sym.Pos.Y model.SymModel with
                        | None -> sym.Pos.Y
                        | Some y -> y
                    let newPos = {X = newX; Y = newY}
                    { sym with
                        Pos = newPos
                        Ports = List.map (fun port -> {port with PortPos = posAdd port.RelativePortPos newPos}) sym.Ports
                        IsDragging = false 
                    }
            )}
        , Cmd.none
    | MouseMove (pos, portOpt) ->
       
        {model with
            SymModel = model.SymModel
            |> List.map (fun sym ->            
                {sym with MouseNear = pos , portOpt }
             
        )}
        , Cmd.none

    | Unselect sId ->
        {model with
            SymModel = model.SymModel
            |> List.map (fun sym ->
                if sym.Id = sId then
                    {sym with 
                       IsSelected = false 
                    }
                else sym
            )}
        , Cmd.none

    | MultipleSelect (sId, pagePos) ->
        {model with
            SymModel = model.SymModel
            |> List.map (fun sym ->
                if sId = sym.Id then
                    {sym with 
                            LastDragPos = pagePos //Zaid
                            IsSelected = true
                    }
                else if sym.IsSelected = true then //Zaid 
                        {sym with 
                            LastDragPos = pagePos 
                        } 
                    else sym
            )}
        , Cmd.none

    | SelectEnclosed (p1,p2) ->

        {model with
             SymModel = model.SymModel
              |> List.map (fun sym ->
                                     let bbox = createBB sym (sym.H, sym.W) 
                                     let box = createSelectBox p1 p2
                                     if bBoxesIntersect bbox box
                                     then {sym with IsSelected = true}
                                     else sym)}
        , Cmd.none

    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private BasicSymbolProps =
    {
        Sym : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

// The following functions are component specific add-ons

let private clkTri (props:BasicSymbolProps) (color:string) _ =
    match props.Sym.Type with
    | DFF | DFFE | Register _ | RegisterE _ | ROM _ | RAM _ -> 

        polygon [
            SVGAttr.Points $"{0.},{props.Sym.H-2.} {0.},{props.Sym.H-16.} {10.},{props.Sym.H-9.}"
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.5
            SVGAttr.Fill color] []
    | _ -> text [] []


let private clkLabel (props:BasicSymbolProps) _ =

    let labelPosX, textAnchor, scaleFactor = 
        match props.Sym.Orientation with
        | Mirror -> (-13., "end", (-1.0, 1.0))
        | _ -> (13., "start", (1.0, 1.0))

    match props.Sym.Type with
    | DFF | DFFE | Register _ | RegisterE _ | ROM _ | RAM _ -> 
        text [ 
            X labelPosX; 
            Y (props.Sym.H-9.); 
            Style [
                UserSelect UserSelectOptions.None
                TextAnchor textAnchor
                DominantBaseline "middle"
                FontSize "13px"
                FontWeight "Bold"
                Fill "Black" 
                Transform (sprintf "translate(%fpx,%fpx) scale(%A) " 0. 0. scaleFactor )
            ]
        ] [str <| "clk"] 
    | _ -> text [] []


let private invertor (props:BasicSymbolProps) (color:string) (rectWidth:float) _ = 
    match props.Sym.Type with
    | Not | Nand | Nor | Xnor -> 
        polygon [
            SVGAttr.Points $"{rectWidth},{(props.Sym.H)/2.} {rectWidth},{(props.Sym.H)/4.} {props.Sym.W},{(props.Sym.H)/2.}"
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.5
            SVGAttr.Fill color] []
    | _ -> text [] []


let private circmaker (sym: Symbol) (i:int) = 

    let port = sym.Ports.[i]

    let pointchecker = containsPoint (createBBMouseHover sym ((sym.CurrentH),(sym.CurrentW ))) (fst sym.MouseNear)
    let opacity =
        match pointchecker with
        |true -> if containsPoint (createBB sym ((sym.CurrentH),(sym.CurrentW ))) (fst sym.MouseNear)
                 then 1.0

                 else  (distFromBB sym.Pos  (sym.CurrentH,sym.CurrentW) (fst sym.MouseNear) )
                      
        |false -> 0.0

    let circleRadius = match snd sym.MouseNear with
                       | Some selPort when selPort.PortType <> port.PortType -> 8.
                       | _ -> 5.

    let circPos : XYPos =
        match sym.Orientation with 
        | Rotate90clk -> {X = port.RelativePortPos.Y ; Y = sym.H - (port.RelativePortPos.X )}
        | Rotate90antiClk -> {X = sym.W - (port.RelativePortPos.Y)  ; Y = (port.RelativePortPos.X )}
        | Mirror -> match  port.ConnectionDirection  with 
                    
                    |Dir.Up -> if port.PortType = PortType.Input then {X = port.RelativePortPos.X ; Y = port.RelativePortPos.Y  }
                                else {X = port.RelativePortPos.X + sym.W  ; Y = port.RelativePortPos.Y  }
        
                    |_ ->  if port.PortType = PortType.Input then {X = port.RelativePortPos.X - sym.W  ; Y = port.RelativePortPos.Y  }
                           else {X = port.RelativePortPos.X + sym.W  ; Y = port.RelativePortPos.Y  }
                    
        | _ -> {X = port.RelativePortPos.X  ; Y = port.RelativePortPos.Y  }

    g      []
        [   if circleRadius = 8. && (calcPointsDist port.PortPos (fst sym.MouseNear) < 10.) then 
                circle
                    [ 
                    Cx circPos.X
                    Cy circPos.Y
                    R 12. 
                    SVGAttr.Fill "lime" 
                    SVGAttr.FillOpacity 0.8
                    SVGAttr.Stroke "Black"
                    SVGAttr.StrokeWidth "0.px"
                    SVGAttr.StrokeOpacity (if sym.IsSelected then 1. else opacity)
                        ] []

            if circleRadius = 5. && (calcPointsDist port.PortPos (fst sym.MouseNear) < 7.) && (snd sym.MouseNear <> None) then 
                circle
                    [ 
                    Cx circPos.X
                    Cy circPos.Y
                    R 8.5
                    SVGAttr.Fill "red"
                    SVGAttr.FillOpacity 0.8
                    SVGAttr.Stroke "Black"
                    SVGAttr.StrokeWidth "0.px"
                    SVGAttr.StrokeOpacity (if sym.IsSelected then 1. else opacity)
                        ] []
            
            if circleRadius = 5. && (calcPointsDist port.PortPos (fst sym.MouseNear) < 9.) && (snd sym.MouseNear = None) then 
                circle
                    [ 
                    Cx circPos.X
                    Cy circPos.Y
                    R 8.5
                    SVGAttr.Fill "blue"
                    SVGAttr.FillOpacity 0.7
                    SVGAttr.Stroke "Black"
                    SVGAttr.StrokeWidth 0
                    SVGAttr.StrokeOpacity (if sym.IsSelected then 1. else opacity)
                        ] []
            
            circle
                [ 
                Cx circPos.X
                Cy circPos.Y
                R circleRadius
                SVGAttr.Fill "Cyan"
                SVGAttr.FillOpacity (if sym.IsSelected then 1. else opacity)
                SVGAttr.Stroke "Black"
                SVGAttr.StrokeWidth "1.75px"
                SVGAttr.StrokeOpacity (if sym.IsSelected then 1. else opacity)
                Style [ZIndex 2]
                    ] []
        ]

let private portLabels (sym:Symbol) (i:int) =
    match sym.Type with
    | Not | And | Or | Xor | Nand | Nor | Xnor 
    | Input _ | Output _ | IOLabel | Constant _ | OldBusSelection _ 
    | MergeWires -> text [] []
    | _ -> 
        let port = sym.Ports.[i]

        let mirrorShift, scaleFactor = 
            match sym.Orientation with
            | Mirror -> (sym.W, (-1.0, 1.0))
            | _ -> (0., (1.0, 1.0))

        let (xMargin, yMargin, textAnchor, dominantBaseline) = 
            match sym.Type with 

            | BusSelection _ -> 
                match port.PortType with
                | PortType.Input -> 
                    match sym.Orientation with
                    | Standard -> (7., -8., "middle", "middle")
                    | Rotate90clk -> (8., 7., "middle", "middle")
                    | Mirror -> (-7., -8., "middle", "middle")
                    | Rotate90antiClk -> (-8., 7., "middle", "middle")
                | PortType.Output -> 
                    match sym.Orientation with
                    | Standard -> (-7., -8., "middle", "middle")
                    | Rotate90clk -> (8., -7., "middle", "middle")
                    | Mirror -> (7., -8., "middle", "middle")
                    | Rotate90antiClk -> (-8., -7., "middle", "middle")


            | SplitWire bw ->
                match sym.Orientation with
                | Standard -> (-7., 9., "middle", "middle")
                | Rotate90clk -> (-9., -7., "middle", "middle")
                | Mirror -> (7., 9., "middle", "middle")
                | Rotate90antiClk -> (9., -7., "middle", "middle")


            | _ ->
                match sym.Orientation with
                | Rotate90clk ->
                    match port.ConnectionDirection with 
                    | Right -> (5., 0., "middle", "auto")
                    | Left -> (-5., 0., "middle", "hanging")
                    | Up -> (0., -6., "end", "middle")
                    | Down -> (0., 6., "start", "middle")
                | Rotate90antiClk ->
                    match port.ConnectionDirection with 
                    | Right -> (5., 0., "middle", "hanging")
                    | Left -> (-5., 0., "middle", "auto")
                    | Up -> (0., 6., "start", "middle")
                    | Down -> (0., -6., "end", "middle")
                | _ ->
                    match port.ConnectionDirection with 
                    | Right -> (5., 0., "start", "middle")
                    | Left -> (-5., 0., "end", "middle")
                    | Up -> (0., -6., "middle", "auto")
                    | Down -> (0., 6., "middle", "hanging")

        let fontSize =
            match sym.Type with 
            | BusSelection _ | SplitWire _ -> "10px"
            | _ -> "13px"

        let labelPos : XYPos =
            match sym.Orientation with 
            | Rotate90clk -> {X = port.RelativePortPos.Y + yMargin ; Y = sym.H - (port.RelativePortPos.X + xMargin)}
            | Rotate90antiClk -> {X = sym.W - (port.RelativePortPos.Y) + yMargin ; Y = (port.RelativePortPos.X + xMargin)}
            | _ -> {X = port.RelativePortPos.X + xMargin ; Y = port.RelativePortPos.Y + yMargin}

        let portLabel =
            match sym.Type with 

            | Mux2 | Mux4 | MuxN _ ->
                let numOfDataPins = 
                    match sym.Type with 
                    | Mux2 -> 2
                    | Mux4 -> 4
                    | MuxN n -> n
                    | _ -> failwithf "should not occur"
                match port.PortType, port.PortNumber with
                | PortType.Input, Some i when i < numOfDataPins -> "i" + string(i)
                | PortType.Input, _ -> "sel" //"s" + string(i-sym.NumOfInputs+sym.NumOfUpwardInputs)
                | PortType.Output, _ -> "out" 

            | Demux2 | Demux4 | DemuxN _ ->
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "in" 
                | PortType.Input, _ -> "sel" //"s" + string(i-sym.NumOfInputs+sym.NumOfUpwardInputs)
                | PortType.Output, _ -> "" + string(i-sym.NumOfInputs)

            | Decode4 -> 
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "SEL"
                | PortType.Input, _ -> "DATA"
                | PortType.Output, _ -> string(i-sym.NumOfInputs)

            | NbitsAdder _ -> 
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "Cin"
                | PortType.Input, Some 1 -> "A"
                | PortType.Input, Some 2 -> "B"
                | PortType.Output, Some 0 -> "Sum"
                | PortType.Output, Some 1 -> "Cout"
                | _ -> failwithf "should not occur"

            | BusSelection (outBW, outLSB) ->
                let inputMSB bw =
                    match bw with 
                    | Some x -> string(x-1)
                    | None -> "?"
                match port.PortType with
                | PortType.Input -> "[" + (inputMSB port.BusWidth) + "..0]"
                | PortType.Output -> "[" + string(outBW + outLSB - 1) + ".." + string(outLSB) + "]"

            | SplitWire upperBW ->
                let outputLSBwidth bw =
                    match bw with 
                    | Some x -> string(x-1)
                    | _ -> failwithf "should not occur"
                match port.PortType, port.PortNumber with
                | PortType.Output, Some 0 -> "[" + (outputLSBwidth port.BusWidth) + "..0]"
                | _ -> ""

            | DFF | DFFE -> 
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "D"
                | PortType.Input, _ -> "EN"
                | PortType.Output, _ -> "Q"

            | Register _ | RegisterE _ -> 
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "data-in"
                | PortType.Input, _ -> "EN"
                | PortType.Output, _ -> "data-out"

            | AsyncROM _ | ROM _ | RAM _ ->
                match port.PortType, port.PortNumber with
                | PortType.Input, Some 0 -> "addr"
                | PortType.Input, Some 1 -> "data-in"
                | PortType.Input, _ -> "write"
                | PortType.Output, _ -> "data-out"

            | Custom features ->
                match port.PortType, port.PortNumber with
                | PortType.Input, Some i -> (fst features.InputLabels.[i])
                | PortType.Output, Some i -> (fst features.OutputLabels.[i])
                | _ -> failwithf "should not occur"

            | _ ->
                match port.PortType with
                | PortType.Input -> "i" + string(i)
                | PortType.Output -> "" + string(i-sym.NumOfInputs)

        text [ 
            X labelPos.X; 
            Y labelPos.Y; 
            Style [
                UserSelect UserSelectOptions.None
                TextAnchor textAnchor
                DominantBaseline dominantBaseline
                FontSize fontSize
                FontWeight "Bold"
                Fill "Black" 
                Transform (sprintf "translate(%fpx,%fpx) scale(%A) " mirrorShift 0. scaleFactor )
            ]
        ] [str <| $"{portLabel}"] 


let private symLabel (sym: Symbol) _ = 

    let symLabel =
        match sym.Type with
        | Input bw | Output bw -> sym.Label + "(" + string(bw-1) + ":0)"
        | _ -> sym.Label

    let mirrorShift, scaleFactor = 
            match sym.Orientation with
            | Mirror -> (sym.W, (-1.0, 1.0))
            | _ -> (0., (1.0, 1.0))

    text [ 
        X (sym.W / 2.); 
        Y (-10.); 
        Style [
            UserSelect UserSelectOptions.None
            TextAnchor "middle"
            DominantBaseline "middle"
            FontSize "13px"
            FontWeight "Bold"
            Fill "Gray" 
            Transform (sprintf "translate(%fpx,%fpx) scale(%A) " mirrorShift 0. scaleFactor )
        ]
    ] [str <| $"{symLabel}"] 


/// Renders a symbols main body
let private renderBasicSymbol =
    FunctionComponent.Of(
        fun (props : BasicSymbolProps) ->

            let numOfPorts = List.length(props.Sym.Ports)

            let fX = props.Sym.Pos.X
            let fY = props.Sym.Pos.Y
            let fW = 
                match props.Sym.Type with
                | Not | Nand | Nor | Xnor -> props.Sym.W - 10.
                | _ -> props.Sym.W
            let fH = props.Sym.H

            let muxCut = 
                match props.Sym.Type with 
                | Mux2 | Mux4 | MuxN _ -> 30. 
                | _ -> 0.

            let demuxCut = 
                match props.Sym.Type with 
                | Demux2 | Demux4 | DemuxN _ -> 30. 
                | _ -> 0.

            // Dynamically allocate up to 9 vertices for a component's main body. If less than 9 are needed, repeat
            // the first point to fill the remaing vertices. The fist and last points will also have a line between them.
            let (p1:XYPos, p2:XYPos, p3:XYPos, p4:XYPos, p5:XYPos, p6:XYPos, p7:XYPos, p8:XYPos, p9:XYPos) =
                match props.Sym.Type with 
                
                | Mux2 | Mux4 | MuxN _ -> 
                    {X = 0.; Y = 0.}, {X = 0.; Y = fH}, {X = fW; Y = fH-muxCut}, {X = fW; Y = muxCut}, {X = 0.; Y = 0.}, 
                    {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}
                
                | Demux2 | Demux4 | DemuxN _ -> 
                    {X = 0.; Y = demuxCut}, {X = 0.; Y = fH-demuxCut}, {X = fW; Y = fH}, {X = fW; Y = 0.}, {X = 0.; Y = demuxCut}, 
                    {X = 0.; Y = demuxCut}, {X = 0.; Y = demuxCut}, {X = 0.; Y = demuxCut}, {X = 0.; Y = demuxCut}
                
                | Input _ ->
                    {X = 0.; Y = 0.}, {X = 0.; Y = fH}, {X = fW-10.; Y = fH}, {X = fW; Y = fH/2.}, {X = fW-10.; Y = 0.}, 
                    {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}

                | Output _ ->
                    {X = 10.; Y = 0.}, {X = 0.; Y = fH/2.}, {X = 10.; Y = fH}, {X = fW; Y = fH}, {X = fW; Y = 0.}, 
                    {X = 10.; Y = 0.}, {X = 10.; Y = 0.}, {X = 10.; Y = 0.}, {X = 10.; Y = 0.}
                
                | IOLabel ->
                    {X = 10.; Y = 0.}, {X = 0.; Y = fH/2.}, {X = 10.; Y = fH}, {X = fW-10.; Y = fH}, {X = fW; Y = fH/2.}, 
                    {X = fW-10.; Y = 0.}, {X = 10.; Y = 0.}, {X = 10.; Y = 0.}, {X = 10.; Y = 0.}

                | Constant _ -> 
                    {X = 0.; Y = 0.}, {X = 0.; Y = fH}, {X = 2.*fW/5.; Y = fH/2.}, {X = fW; Y = fH/2.}, {X = 2.*fW/5.; Y = fH/2.}, 
                    {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}

                | OldBusSelection _ -> 
                    {X = 0.; Y = 0.}, {X = 0.; Y = fH}, {X = fW/2.; Y = fH}, {X = 3.*fW/4.; Y = 4.*fH/5.}, {X = fW; Y = 4.*fH/5.}, 
                    {X = fW; Y = fH/5.}, {X = 3.*fW/4.; Y = fH/5.}, {X = fW/2.; Y = 0.}, {X = 0.; Y = 0.}
  
                | BusSelection _ -> 
                    {X = 0.; Y = fH/2.}, {X = fW; Y = fH/2.}, {X = fW/2.; Y = fH/2.}, {X = fW/2.; Y = 0.}, {X = fW/2.; Y = fH}, 
                    {X = fW/2.; Y = fH/2.}, {X = 0.; Y = fH/2.}, {X = 0.; Y = fH/2.}, {X = 0.; Y = fH/2.}

                | MergeWires -> 
                    {X = 0.; Y = 0.}, {X = fW/2.; Y = 0.}, {X = fW/2.; Y = fH/2.}, {X = fW; Y = fH/2.}, {X = fW/2.; Y = fH/2.}, 
                    {X = fW/2.; Y = fH}, {X = 0.; Y = fH}, {X = fW/2.; Y = fH}, {X = fW/2.; Y = 0.}

                | SplitWire _ -> 
                    {X = fW; Y = 0.}, {X = fW/2.; Y = 0.}, {X = fW/2.; Y = fH/2.}, {X = 0.; Y = fH/2.}, {X = fW/2.; Y = fH/2.}, 
                    {X = fW/2.; Y = fH}, {X = fW; Y = fH}, {X = fW/2.; Y = fH}, {X = fW/2.; Y = 0.}

                | _ -> 
                    {X = 0.; Y = 0.}, {X = 0.; Y = fH}, {X = fW; Y = fH}, {X = fW; Y = 0.}, {X = 0.; Y = 0.}, 
                    {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}, {X = 0.; Y = 0.}


            let headerPositionY = 
                match props.Sym.Type with 
                | Constant _ -> 20.
                | Mux2 | Mux4 | MuxN _ -> muxCut + 5.
                | Demux2 | Demux4 | DemuxN _ -> demuxCut + 5.
                | _ -> 15.

            let headerTextAnchor =
                match props.Sym.Type with
                | Constant _ -> "start"
                | _ -> "middle"

            let headerFontSize =
                match props.Sym.Type with
                | BusSelection _ -> "10px"
                | _ -> "13px"

            let header = 
                match props.Sym.Type with
                | Not -> "1"
                | And | Nand -> "&"
                | Or | Nor -> "≥1"
                | Xor | Xnor -> "=1"
                | Decode4 -> "Decode"
                | NbitsAdder bw -> "Adder (" + string(bw-1) + ":0)" 
                | Constant (_,v) -> System.Convert.ToString(v, 16)
                | OldBusSelection (bw,lsb) -> 
                    if bw = 1 
                    then string(lsb)
                    else "[" + string(bw+lsb-1) + ".." + string(lsb) + "]"
                | Mux2 | Mux4 | MuxN _ -> "Mux"
                | Demux2 | Demux4 | DemuxN _ -> "Demux"
                | DFF -> "DFF"
                | DFFE -> "DFFE"
                | Register bw | RegisterE bw -> "REG" + string(bw)
                | AsyncROM _ -> "Async-ROM"
                | ROM _ -> "ROM"
                | RAM _ -> "RAM"
                | Custom features -> features.Name
                | _ -> ""

            let color =
                if props.Sym.IsSelected then
                    "#aacbff"
                else
                    "lightgray"

            let scaleFactor, rotation =
                match props.Sym.Orientation with
                | Standard -> (1.0, 1.0), 0
                | Mirror -> (-1.0, 1.0), 0
                | Rotate90clk -> (1.0, 1.0), 90
                | Rotate90antiClk -> (1.0, 1.0), -90

            let mirrorShift : float =
                match scaleFactor with 
                | (1.0, 1.0) -> 0.
                | (-1.0, 1.0) -> fW //props.Sym.W
                | _ -> failwithf "should not occur"

            let rotateShift : XYPos =
                match rotation with
                | 0 -> {X = 0.; Y = 0.}
                | 90 -> {X = fH; Y = 0.}  
                | -90 -> {X = 0.; Y = props.Sym.W} 
                | _ -> failwithf "should not occur"

            g   [ Style [ 
                TransformOrigin "0px 0px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%A) " (fX+mirrorShift+rotateShift.X) (fY+rotateShift.Y) rotation scaleFactor )
                ]
                ]
 
                ([
                    polygon 
                        [
                            SVGAttr.Points $"{p1.X},{p1.Y} {p2.X},{p2.Y} {p3.X},{p3.Y} {p4.X},{p4.Y} {p5.X},{p5.Y} {p6.X},{p6.Y} {p7.X},{p7.Y} {p8.X},{p8.Y} {p9.X},{p9.Y}"
                            //SVGAttr.Points $"{cutLeftW},{cutLeftH} {vertex5.X},{vertex5.Y} {cutLeftW},{fH-cutLeftH} {fW-cutRightW},{fH-cutRightH} {vertex6.X},{vertex6.Y} {fW-cutRightW},{cutRightH}"
                            SVGAttr.StrokeWidth "2px"
                            SVGAttr.Stroke "Black"
                            SVGAttr.FillOpacity 1.
                            SVGAttr.Fill color
                            Style[ ZIndex 1]
                            ] []

                    text [ 
                        X (fW/2.); 
                        Y headerPositionY; 
                        Style [
                            UserSelect UserSelectOptions.None
                            TextAnchor headerTextAnchor
                            DominantBaseline "middle" 
                            FontSize headerFontSize
                            FontWeight "Bold"
                            Fill "Black" 
                            Transform (sprintf "translate(%fpx,%fpx) scale(%A) " mirrorShift 0. scaleFactor )
                        ]
                    ] [str <| sprintf $"{header}"] 

            ] 
            @ List.map (symLabel props.Sym) [0]
            @ List.map (portLabels props.Sym) [0..numOfPorts-1]
            @ List.map (invertor props color fW) [0]
            @ List.map (clkTri props color) [0]
            @ List.map (clkLabel props) [0]
            @ List.map (circmaker props.Sym) [0..numOfPorts-1]
            )
    , "BasicSymbol"
    , equalsButFunctions
    )



/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) : ReactElement = 
    model.SheetSymbol @ (List.rev model.SymModel)
    |> List.map (  fun sym -> renderBasicSymbol {    Sym = sym 
                                                     Dispatch = dispatch      
                                                     Key = string(sym.Id)  
                                                     })
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.SymbolId) : XYPos = 
    List.find (fun (sym:Symbol) -> sym.Id = sId) symModel.SymModel
    |> (fun sym -> sym.Pos)


/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymModelWithComponent (symModel: Model) (comp:CommonTypes.Component) : list<Component> =
    if List.tryFind (fun (sym:Symbol) -> sym.Id = comp.Id) symModel.SymModel = None
    then comp :: symModel.SymModel
    else symModel.SymModel



//----------------------interface to Issie-----------------------------//

let extractComponent //Issie
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component = 
    List.find (fun (sym:Symbol) -> sym.Id = SymbolId(string(sId))) symModel.SymModel


let extractComponents (symModel: Model) : CommonTypes.Component list = symModel.SymModel // issie


/// Looks up a symbol using its Id
let getsymbolFromSymbolId (symbolId: SymbolId) (symModel: Model) : Symbol = // issie
    List.find (fun (sym:Symbol) -> sym.Id = symbolId) symModel.SymModel



//----------------------interface to BusWire----------------------------//

// /// Outputs a map containing a symbol's ports, with their respective Ids as the keys
// let getPortsFromSymbol (symbol: Symbol) : Map<string,Port> =
//     symbol.Ports
//     |> List.map (fun port -> (port.Id,port))
//     |> Map.ofList 

// let getPortFromPortId (portId: PortId) (SymModel: Model) : Port =
//     List.find (fun (p:Port) -> p.Id = portlId) SymModel


let getPortsFromSymbol (symbol: Symbol) : list<Port> = symbol.Ports // Wire


let getPosFromPort (port : Port) : XYPos = port.PortPos //Wire

let getPortTypeFromPort (port : Port) : PortType = port.PortType // Wire

let getPortIdFromPort (port: Port) : string = port.Id //Wire


/// Returns the side of the symbol that the port is on 
let getDirFromPort (port : Port) : Dir = // Wire
    match port.ConnectionDirection with 
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up


/// Returns the BusWidth of a port
let getWidthFromPort (port : Port) : int =  //Wire
    match port.BusWidth with
    | Some x -> x
    | None -> failwithf "should not occur"


/// Returns all the ports connected to the symbol with the specified Id
let getPortsFromId (symbolId : SymbolId) (symModel : Model) : Map<string,Port> = // Wire
    let sym = List.find (fun (sym:Symbol) -> sym.Id = symbolId) symModel.SymModel
    sym.Ports
    |> List.map (fun port -> (port.Id,port))
    |> Map.ofList 


let getSelectedSymbolList (model : Model) : list<SymbolId> = //Wire
    model.SymModel
    |> List.filter (fun s -> s.IsSelected)
    |> List.map (fun s -> s.Id)

let getPortsOfSelectedSymbolList (model : Model) : list<string> = // Wire
    model.SymModel
    |> List.filter (fun s -> if s.IsSelected then 
                                true 
                             else false)
    |> List.collect (fun s -> s.Ports)
    |> List.map (fun p -> p.Id)

let getPortsMapOfSelectedSymbolList (model : Model) : Map<string,Port>  = // wire
    model.SymModel
    |> List.filter (fun s -> if s.IsSelected then 
                                true 
                             else false)
    |> List.collect (fun s -> s.Ports)
    |> List.map (fun p -> (p.Id,p))
    |> Map.ofList

    //----------------------interface to Sheet----------------------------//


let getSymbolBBox (symbol: Symbol) : BB = // SHeet
    boxOf (posAdd symbol.Pos (posOf 20.0 20.0)) (posDiff symbol.Pos (posOf 20.0 20.0))


let createPortBB (port: Port) (x: float) : BB = // sheet
    {
        TopLeft = {X = port.PortPos.X - x ; Y = port.PortPos.Y - x }
        BottomRight = {X = port.PortPos.X + x ; Y = port.PortPos.Y + x}
    }

let FindPort (mousePos: XYPos) (model: Model) : option<Port * PortType> =  // Sheet
    let s = model.SymModel |> List.map (fun sym -> 
                            match List.tryFind (fun p -> containsPoint (createPortBB p 10.) mousePos) sym.Ports with 
                            | Some p -> Some (p, p.PortType)
                            | None -> None
                        )  
    match List.filter (fun x -> x <> None) s with 
    | [] -> None 
    | s -> s.[0]

