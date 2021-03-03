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

    // | IOLabel -> 
    // | BusSelection(outWidth,lsb) -> 
    // | BusCompare(busWidth,compWith) -> 
    // | Constant (width, constValue) -> 
    // | Not | And | Or | Xor | Nand | Nor | Xnor  -> 
    // | Decode4 ->
    // | Mux2 | Demux2 -> 
    // | NbitsAdder width -> 
    // | DFF -> 
    // | DFFE -> 
    // | Register width | RegisterE width -> 
    // | AsyncROM mem| ROM mem -> 
    // | RAM mem -> 

//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let fstof3 (a, _, _) = a


let secondof3 (_, b, _) = b


let thirdof3 (_, _, c) = c



let makePortLabels (width : float) (height : float) (ports: Port list) (portNum : int)=
    let pName = ports.[portNum].PortName
    let portpos = ports.[portNum].PortPos
    let buswidth = ports.[portNum].BusWidth

    
    //let yPosition = float(portNum+1)/float (List.length ports + 1 ) * height  


    match ports.[portNum].PortDirection,ports.[portNum].PortName with 
          | Down, "S" ->text [ 
                               
                                X (width/2.);
                                Y 37.5 ;
                                Style [

                                    TextAnchor "middle"
                                    DominantBaseline "middle" 
                                    FontSize "10px"
                                    FontWeight "Bold"
                                    Fill "black" 
                                ]
                            ] [str <| sprintf "%A" pName ] 


          | Down, "EN" ->text [ 
                               
                                X ( width / 2.);
                                Y (height-3.);
                                Style [
                                    
                                    TextAnchor "middle"
                                    DominantBaseline "bottom" 
                                    FontSize "10px"
                                    FontWeight "Bold"
                                    Fill "black" 
                                ]
                            ] [str <| sprintf "%A" pName ] 
  
          | _ ->text [ 
                        if ports.[portNum].PortType = PortType.Input then X 3. ;
                        else 
                            X ( width - 3.);
                        if (List.map (fun  port -> port.PortName) ports |> List.contains "EN") || (List.map (fun  port -> port.PortName) ports |> List.contains "S") 
                            then  Y (float(portNum+1)/float (List.length ports ) * height);
                        else  Y (float(portNum+1)/float (List.length ports + 1 ) * height) ;
     
                        Style [
                            //change both to left and end when done
                            if ports.[portNum].PortType = PortType.Input then TextAnchor "left"
                            else
                                TextAnchor "end"
                            DominantBaseline "middle" 
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "black" 
                        ]
                    ] [str <| sprintf "%A" pName] //sprintf "%A,%A"  portpos.X portpos.Y


let lookupPortLabels (compType:ComponentType) (porttype: PortType) (index:int) =
    match compType with 
    | Input busWidth | Output busWidth -> " "
    | IOLabel -> " "
    | BusSelection(outWidth,lsb) -> " "
    | BusCompare(busWidth,compWith) -> " " 
    | Constant (width, constValue) -> " "
    | Not | And | Or | Xor | Nand | Nor | Xnor  -> " "
    | Decode4 -> if porttype = PortType.Input then 
                    match index with
                    | 0 -> "sel"
                    | 1 -> "data"
                    | _ -> " "
                 else 
                    match index with
                    | a -> $"{a}"

    
    | Mux2 ->if porttype = PortType.Input then 
                match index with
                | 0 -> "0"
                | 1 -> "1"
                | _ -> "S"
             else 
                " "
 
    | Demux2 ->if porttype = PortType.Output then 
                  match index with
                  | 0 -> "0"
                  | 1 -> "1"
                  | _ -> " "

               else 
                  match index with
                  | 0 -> " "
                  | 1 -> "S"
                  | _ -> " "
 
    | NbitsAdder width ->  if porttype = PortType.Input then 
                              match index with
                              | 0 -> "Cin"
                              | 1 -> "A"
                              | 2 -> "B"
                              | _ -> " "
                           else 
                              match index with
                              | 0 -> "Sum"
                              | 1 -> "Cout"
                              | _ -> " "
    | DFF ->   if porttype = PortType.Input then 
                  match index with
                  | 0 -> "D"
                  | _ -> " "

               else 
                  match index with
                  | 0 -> "Q"
                  | _ -> " "
    | DFFE ->  if porttype = PortType.Input then 
                  match index with
                  | 0 -> "D"
                  | 1 -> "EN"
                  | _ -> " "

               else 
                  match index with
                  | 0 -> "Q"
                  | _ -> " "

    | Register width-> if porttype = PortType.Input then 
                          match index with
                          | 0 -> "data_in"
                          | _ -> " "
                       else 
                          match index with
                          | 0 -> "data_out"
                          | _ -> " "

    | RegisterE width->if porttype = PortType.Input then 
                          match index with
                          | 0 -> "data_in"
                          | 1 -> "EN"
                          | _ -> " "
                       else 
                          match index with
                          | 0 -> "data_out"
                          | _ -> " "


    | AsyncROM mem| ROM mem -> if porttype = PortType.Input then 
                                  match index with
                                  | 0 -> "addr"
                                  | _ -> " "
                               else 
                                  match index with
                                  | 0 -> "data"
                                  | _ -> " "
    
    | RAM mem -> if porttype = PortType.Input then 
                    match index with
                    | 0 -> "addr"
                    | 1 -> "data_in"
                    | 2 -> "write"
                    | _ -> " "
                 else 
                    match index with
                    | 0 -> "data_out"
                    | _ -> " "
    | SplitWire busWidth -> " "
    | MergeWires -> " "

    |_-> "test"



let fillPortFields (compType:ComponentType) (uuid:string) (w: float) (h: float) (pos:XYPos) (porttype: PortType) (buswidth: int) (indexLst:int list) =
    
    let portmaker index = 
        
        let portsPlus1 = float((List.length indexLst) + 1) // +1 added to adjust port positions
        let indexPlus1 = float(index + 1)      
        let portname = lookupPortLabels compType porttype index
        {
        Id = Helpers.uuid()
        PortName = portname

        PortPos = match compType,portname with
                  | _,"EN" -> posOf (pos.X + (float w/2.)) (pos.Y+ (float h))

                  | _,"S"  -> posOf (pos.X + (float w)/2.) (pos.Y+ (float h)*3./4.)
                  
                  | Demux2,_ | Mux2,_ | DFFE,_-> if porttype = PortType.Input
                                                 then posOf pos.X (pos.Y+ indexPlus1/(portsPlus1-1.)*(float h)) 
                                                 else posOf (pos.X + (float w)) (pos.Y+ indexPlus1/portsPlus1*(float h))
                  | RegisterE width ,_ -> if porttype = PortType.Input
                                          then posOf pos.X (pos.Y+ indexPlus1/(portsPlus1-1.)*(float h)) 
                                          else posOf (pos.X + (float w)) (pos.Y+ indexPlus1/portsPlus1*(float h))
                  
                  | SplitWire width,_ -> if porttype = PortType.Input
                                         then posOf pos.X (pos.Y+ indexPlus1/portsPlus1* h) 
                                         else posOf (pos.X + w) (pos.Y + float(index)*h) 
  
                  | MergeWires,_ -> if porttype = PortType.Output
                                         then posOf (pos.X + w) (pos.Y+ indexPlus1/portsPlus1* h) 
                                         else posOf pos.X (pos.Y + float(index)*h) 

                  | _ ->  if porttype = PortType.Input
                          then posOf pos.X (pos.Y + indexPlus1/portsPlus1* h) 
                          else posOf (pos.X + w) (pos.Y + indexPlus1/portsPlus1* h) 
                
        
        
        PortDirection = match porttype,portname with 
                        | PortType.Input,"S" -> Down
                        | PortType.Input,"EN" -> Down
                        | PortType.Output,_ -> Right
                        | _-> Left
        
        //matching needed where inputs/output ports have different widths
        BusWidth = match compType,index,porttype with
                   | RAM mem, 1, PortType.Input -> Some mem.WordWidth
                   | RAM mem, 2, PortType.Input -> Some 1
                   | NbitsAdder w, 0, PortType.Input -> Some 1
                   | NbitsAdder w, 1, PortType.Output -> Some 1
                   | RegisterE w, 1 , PortType.Input -> Some 1
                   | IOLabel,_,_ -> None
                   | MergeWires,_,_ -> None
                   | SplitWire w,_,PortType.Input -> None 
                   | SplitWire w,1,PortType.Output -> None 
                   |_  -> Some buswidth
        
        PortNumber = Some index
        PortType = porttype   
        HostId = uuid}
    
    indexLst
    |> List.map portmaker




//return a tuple of input and output list
let makeSymbolPorts (compType:ComponentType) (uuid:string) (w: float) (h: float) (pos:XYPos)  =
    match compType with 
    | Input busWidth -> [],  fillPortFields compType uuid w h pos PortType.Output busWidth [0]
    | Constant (width, constValue) -> [],  fillPortFields compType uuid w h pos PortType.Output width [0]
    | Output busWidth ->  fillPortFields compType uuid w h pos PortType.Input busWidth [0], []
    | BusCompare(busWidth,compWith) ->  fillPortFields compType uuid w h pos PortType.Input busWidth [0],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | BusSelection(outWidth,lsb) ->  fillPortFields compType uuid w h pos PortType.Input (outWidth+lsb) [0],  fillPortFields compType uuid w h pos PortType.Output outWidth [0]
    | IOLabel | Not | DFF  ->  fillPortFields compType uuid w h pos PortType.Input 1 [0],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | DFFE -> fillPortFields compType uuid w h pos PortType.Input 1 [0..1],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | And | Or | Xor | Nand | Nor | Xnor  -> fillPortFields compType uuid w h pos PortType.Input 1 [0;1],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | Decode4 ->  fillPortFields compType uuid w h pos PortType.Input 1 [0..1],  fillPortFields compType uuid w h pos PortType.Output 1 [0..3]
    
    | Mux2 ->  fillPortFields compType uuid w h pos PortType.Input 1 [0..2],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | Demux2 ->  fillPortFields compType uuid w h pos PortType.Input 1 [0..1],  fillPortFields compType uuid w h pos PortType.Output 1 [0..1]
    | NbitsAdder width ->  fillPortFields compType uuid w h pos PortType.Input width [0..2],  fillPortFields compType uuid w h pos PortType.Output width [0..1]
    
    | Register width ->  fillPortFields compType uuid w h pos PortType.Input width [0],  fillPortFields compType uuid w h pos PortType.Output width [0]
    | RegisterE width ->  fillPortFields compType uuid w h pos PortType.Input width [0..1],  fillPortFields compType uuid w h pos PortType.Output width [0]  
    | AsyncROM mem | ROM mem ->  fillPortFields compType uuid w h pos PortType.Input mem.AddressWidth [0],  fillPortFields compType uuid w h pos PortType.Output mem.WordWidth [0]
    | RAM mem -> fillPortFields compType uuid w h pos PortType.Input mem.AddressWidth [0..2],  fillPortFields compType uuid w h pos PortType.Output mem.WordWidth [0]
    
    | MergeWires -> fillPortFields compType uuid w h pos PortType.Input 1 [0..1],  fillPortFields compType uuid w h pos PortType.Output 1 [0]
    | SplitWire width -> fillPortFields compType uuid w h pos PortType.Input 1 [0],  fillPortFields compType uuid w h pos PortType.Output width [0..1]


    |_-> [],[]



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








//-----------------------Symbol Parameters---------------------//
//createNewSymbol functions
let makeNameId (compType: ComponentType) :string*float*float =
    match compType with 
    | Input busWidth -> (" ",0.,0.)
    | Output busWidth -> (" ",0.,0.)
    | IOLabel -> (" ",0.,0.)
    | BusSelection(outWidth,outputLSBit) ->($"[{outWidth+outputLSBit-1}..{outputLSBit}]",17.,15.)
    | BusCompare (busWidth,compareWith) ->($"={compareWith}",17.,15.)
    | Constant (width, constValue) -> ($"{constValue}",25.,5.)
    | Not -> ("1",20.,20.)
    | And ->("&",20.,20.)
    | Or -> ("≥1",20.,20.)
    | Xor -> ("=1",20.,20.)
    | Nand -> ("$",20.,20.)
    | Nor -> ("≥1",20.,20.)
    | Xnor -> ("=1",20.,20.)
    | Decode4 -> ("decode",40.,10.)
    | Mux2 | Demux2 -> (" ", 0. , 0.)
    | NbitsAdder width -> ($"adder ({width-1}..0)",50.,10.)
    | DFF -> ("DFF",25.,10.)
    | DFFE -> ("DFFE",50.,10.)
    | Register width -> ($"{width} bit REG",75.,10.)
    | RegisterE width -> ($"{width} bit REG_E",75.,10.)
    | AsyncROM mem -> ("Async-ROM",40.,10.)
    | ROM mem -> ("ROM",40.,10.)
    | RAM mem -> ("RAM",70.,10.)
    |_-> (" ",0.,0.)


let addSymbolValues  (compType: ComponentType) : float * float * (string * float * float) * Square =
    
    match compType with
    | Input busWidth| Output busWidth -> (30.,20.,makeNameId compType,No)
    | IOLabel -> (30.,20.,makeNameId compType,No)
    | BusSelection(outWidth,lsb) -> (40.,30.,makeNameId compType,No)
    | BusCompare(busWidth,compWith) -> (40.,30.,makeNameId compType,No)
    | Constant (width, constValue) -> (40.,20.,makeNameId compType,No)
    | Not | And | Or | Xor | Nand | Nor | Xnor  -> (40.,40.,makeNameId compType,Yes)
    | Decode4 -> (80.,120.,makeNameId compType,Yes)
    | Mux2 | Demux2 -> (30.,50.,makeNameId compType,No)
    | NbitsAdder width -> (100.,115.,makeNameId compType,Yes)
    | DFF -> (50.,50.,makeNameId compType,Yes)
    | DFFE -> (100.,50.,makeNameId compType,Yes)
    | Register width | RegisterE width -> (150.,85.,makeNameId compType,Yes)
    | AsyncROM mem| ROM mem -> (80.,100.,makeNameId compType,Yes)
    | RAM mem -> (140.,100.,makeNameId compType,Yes)
    | MergeWires -> (40.,40.,makeNameId compType,No)
    | SplitWire w -> (40.,40.,makeNameId compType,No)
    |_-> (100.,100.,(" ",25.,25.),No)



//to make triangles and polygons of non-square shapes
let makePolygon   (compType: ComponentType) (w:float) (h:float) : (float * float) * (float * float) * (float * float) * (float * float) * (float * float) =
    match compType with 
    | Input busWidth -> (0.,0.) , (w-10.,0.) , (w,h/2.) , (w-10.,h) , (0.,h)
    | Output busWidth -> (10.,0.) , (w,0.) , (w,h) , (10.,h) , (0.,h/2.0)
    | IOLabel -> (0.,h/2.) , (10.,h) , (20.,h) , (w,h/2.) , (w/2.,0.)
    | BusSelection(busWidth,lsb) -> (0.,0.),(w-10.,5.),(w,h/2.),(w-10.,h-5.),(0.,h)
    | BusCompare(outWidth,lsb) -> (0.,0.),(w-10.,5.),(w,h/2.),(w-10.,h-5.),(0.,h)
    | Constant(width, constVal) -> (0.,0.),(15.,h/2.),(w,h/2.),(15.,h/2.),(0.,h)
    | Not | Nand | Nor | Xnor  -> (w,h/2.0),(w,h/2.0-10.0),(w+10.0,h/2.0),(w,h/2.0),(w,h/2.0-10.0)  
    | Mux2 -> (0.,0.),(w,15.),(w,35.),(0.,h),(0.,0.)
    | Demux2 -> (0.,15.),(w,0.),(w,h),(0.,35.),(0.,15.)   
    | DFF | DFFE -> (0.,38.),(7.,43.),(0.,48.),(0.,38.),(7.,43.)
    | Register width | RegisterE width -> (0.,73.),(7.,78.),(0.,83.),(0.,73.),(7.,78.)
    | RAM mem | ROM mem -> (0.,88.),(7.,93.),(0.,98.),(0.,88.),(7.,93.)
    | MergeWires -> (w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.)
    | SplitWire width -> (w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.),(w/2.,h/2.)
    |_-> (0.,0.),(0.,0.),(0.,0.),(0.,0.),(0.,0.) //does not show for other symbols


 
//Create Symbol Func
let createNewSym  (compType: ComponentType) (label: string) (pagePos: XYPos)=
    let tempId = Helpers.uuid()
    let (w,h,nameid,isSqr) = addSymbolValues compType
    let (p1,p2,p3,p4,p5) = makePolygon compType w h
    
    let inPorts,outPorts = makeSymbolPorts compType tempId w h pagePos
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
        Pcords = p1,p2,p3,p4,p5
        IsSquare = isSqr

        InputPorts = inPorts
            
        OutputPorts = outPorts

    }




/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with 
    | AddComp (compType, label ,pagePos) ->
        createNewSym  compType label pagePos ::model,Cmd.none



    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none

    | UpdateSymbolModelWithComponent  comp ->
        List.map (fun (sym : Symbol) -> if sym.Id = comp.Id then comp else sym) model
         
        , Cmd.none

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
    let data : Map<int64,int64> = Map.empty
    let mem : Memory = {AddressWidth = 10; WordWidth = 10; Data = data}

    let model = 
        List.allPairs [1] [1]
        |> List.map (fun (x,y) -> {X = float (x*100+30); Y=float (y*150)})
        |> List.map (createNewSym testcomp "4 bit Decoder")

    
    let model1,cmd1 = update (AddComp (  And ,"And1", posOf 100. 30.) ) (model : Model)
    let model2,cmd2 = update (AddComp (  Nor ,"Nor1", posOf 200. 30.) ) (model1 : Model)
    let model3,cmd3 = update (AddComp (  IOLabel ,"Label1", posOf 300. 30.) ) (model2 : Model)
    let model4,cmd4 = update (AddComp (  BusSelection(5,5) ,"Bus Select", posOf 350. 50.) ) (model3 : Model)
    let model5,cmd5 = update (AddComp (  BusCompare(5,5) ,"Bus Compare", posOf 350. 100.) ) (model4 : Model)
    let model6,cmd6 = update (AddComp (  Input 5 ,"Input [4:0]", posOf 150. 500.) ) (model5 : Model)
    let model7,cmd7 = update (AddComp (  Output 5 ,"Output [4:0]", posOf 250. 500.) ) (model6 : Model)
    let model8,cmd8 = update (AddComp (  Mux2 ,"Mux2", posOf 10. 60.) ) (model7 : Model)
    let model9,cmd9 = update (AddComp (  Demux2 ,"Demux2", posOf 10. 110.) ) (model8 : Model)
    let model10,cmd10 = update (AddComp (  NbitsAdder 5 ,"5 bit Adder", posOf 10. 150.) ) (model9 : Model)
    let model11,cmd11 = update (AddComp (  DFF ,"DFF", posOf 10. 270.) ) (model10 : Model)
    let model12,cmd12 = update (AddComp (  DFFE ,"DFF with Enable", posOf 10. 340.) ) (model11 : Model)
    let model13,cmd13 = update (AddComp (  Register 5 ,"5 bit Reg", posOf 150. 300.) ) (model12 : Model)
    let model14,cmd14 = update (AddComp (  RegisterE 6 ,"RegE", posOf 150. 400.) ) (model13 : Model)
    
    let model15,cmd15 = update (AddComp (  AsyncROM mem ,"Async ROM", posOf 350. 300.) ) (model14 : Model)
    let model16,cmd16 = update (AddComp (  ROM mem ,"ROM", posOf 450. 300.) ) (model15 : Model)
    let model17,cmd17 = update (AddComp (  RAM mem ,"RAM", posOf 550. 300.) ) (model16 : Model)

    let model18,cmd18 = update (AddComp (  SplitWire 5 ,"splitwire", posOf 450. 450.) ) (model17 : Model)
    let model19,cmd19 = update (AddComp (  MergeWires ,"mergewires", posOf 375. 450.) ) (model18 : Model)

    sprintf $"{(model19.[0]).InputPorts}" |> ignore

    update (AddComp (  Constant(5,5) ,"Constant Val = 5 ", posOf 10. 10.) ) (model19 : Model)


//----------------------------View Function for Symbols----------------------------//


type private RenderCompProps =
    {
        Sym : Symbol 
        Dispatch : Dispatch<Msg>
        key: string
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderComponent =
    FunctionComponent.Of(
        fun (props : RenderCompProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    
                    Dragging(props.Sym.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Sym.IsDragging then
                    "red"
                else
                    "skyblue"

            let x = props.Sym.Pos.X
            let y = props.Sym.Pos.Y
            let height = props.Sym.H
            let width = props.Sym.W
            let isSquare =  props.Sym.IsSquare
            let (t1x,t1y),(t2x,t2y),(t3x,t3y),(t4x,t4y),(t5x,t5y) = props.Sym.Pcords
            let comptype = props.Sym.Type

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
                            if isSquare = Yes then
                                SVGAttr.Points $"{0},{0} {width},{0} {width},{height} {0},{height}" 
                                SVGAttr.StrokeWidth "1px"
                                SVGAttr.Stroke "black"
                                SVGAttr.FillOpacity 0.7
                                SVGAttr.Fill color
                            else 
                                SVGAttr.Points $"{t1x},{t1y} {t2x},{t2y} {t3x},{t3y} {t4x},{t4y} {t5x},{t5y}" 
                                SVGAttr.StrokeWidth "1px"
                                SVGAttr.Stroke "black"
                                SVGAttr.FillOpacity 0.7
                                SVGAttr.Fill color
                            ] []

                    //makes additional polygon shapes for square symbols  
                    if isSquare = Yes then
                        polygon
                            [ 
                                
                                SVGAttr.Points $"{t1x},{t1y} {t2x},{t2y} {t3x},{t3y} {t4x},{t4y} {t5x},{t5y}" 
                                SVGAttr.StrokeWidth "1px"
                                SVGAttr.Stroke "black"
                                SVGAttr.FillOpacity 0.7
                                SVGAttr.Fill color] []
                    
                    //Name Id for various components
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
                    
                    //make the label
                    text [          
                        X (width/2.); 
                        Y -10.;
                        Style [
                            TextAnchor "middle" 
                            DominantBaseline "middle" 
                            FontSize "13px"
                            FontWeight "Normal"
                            Fill "Black" 
                        ]
                    ] [str <| sprintf $"{props.Sym.Label}" ]

                    //add clk to clocked blocks
                    let makeclktxt string1 =  text [          
                                                    X 15.; 
                                                    Y (height-7.);
                                                    Style [
                                                        TextAnchor "middle" 
                                                        DominantBaseline "middle" 
                                                        FontSize "10px"
                                                        FontWeight "bold"
                                                        Fill "Black" 
                                                    ]
                                               ] [str <| sprintf $"{string1}" ]
                    
                    //line maker for merge/split wire
                    let mergewiremaker  = polyline [
                                            SVGAttr.Points $"{0.},{0.} {width/2.0},{0} {width/2.},{height/2.} {width},{height/2.} {width/2.},{height/2.} {width/2.},{height} {0.},{height}" 
                                            SVGAttr.StrokeWidth "1px"
                                            SVGAttr.Stroke "black"
                                            SVGAttr.FillOpacity 0.0

                                          ] []
                    let splitwiremaker  = polyline [
                                            SVGAttr.Points $"{0.},{height/2.} {width/2.},{height/2.} {width/2.},{0.} {width},{0.} {width/2.},{0.} {width/2.},{height} {width},{height}" 
                                            SVGAttr.StrokeWidth "1px"
                                            SVGAttr.Stroke "black"
                                            SVGAttr.FillOpacity 0.0

                                          ] []
                    
                    match comptype with 
                    | DFF | DFFE -> makeclktxt "clk"
                    | Register w1 | RegisterE w1-> makeclktxt "clk"
                    | ROM mem | RAM mem -> makeclktxt "clk"
                    | SplitWire w -> splitwiremaker
                    | MergeWires -> mergewiremaker 
                    |_-> makeclktxt " "
                    
                    //match comptype with 
                    //| SplitWire w -> linemaker 0. 0. 10. 10. ,linemaker 0. 0. 10. 10. ,linemaker 0. 0. 10. 10.,linemaker 0. 0. 10. 10.,linemaker 0. 0. 10. 10.
                    //| _-> makeclktxt " ",makeclktxt " ",makeclktxt " ",makeclktxt " ",makeclktxt " "



            ] @ List.map (makePortLabels  width height props.Sym.InputPorts) [0..(List.length props.Sym.InputPorts-1)]
            @ List.map (makePortLabels width height props.Sym.OutputPorts) [0..(List.length props.Sym.OutputPorts-1)])
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



let getPosFromPort (port : Port) : XYPos =  // Returns the coordinates of a port
    port.PortPos

let getDirFromPort (port : Port) : PortDirection = // Returns the side of the symbol that the port is on (Up,Down,Left,Right)
    port.PortDirection

let getWidthFromPort (port : Port) : int =  // Returns the BusWidth of a port
    match port.BusWidth with
    | Some width-> width
    | None -> failwithf "No specified width"

let getPortsFromId (symbolId : SymbolId) (symbolModel : Model) : Map<string,Port> = // Returns all the ports connected to the symbol with the specified Id
    symbolModel
    |> List.find (fun sym -> sym.Id = symbolId )
    |> fun sym-> (sym.InputPorts @ sym.OutputPorts)
    |> List.map (fun p -> (p.Id,p))
    |> Map.ofList







/// Update the symbol with matching  SymbolId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
   
    match List.tryFind (fun (sym:Symbol) -> sym.Id = comp.Id) symModel  with
    | Some sym -> List.map (fun (sym : Symbol) -> if sym.Id = comp.Id then comp else sym) symModel
    | None -> comp::symModel



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
        (sId:CommonTypes.SymbolId) : CommonTypes.Component= 

    List.find (fun (sym:Symbol) -> sym.Id = sId) symModel

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    symModel