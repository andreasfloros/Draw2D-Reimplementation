module Helpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag

type MouseT = {
    Pos: XYPos
    Op: MouseOp}

type BB = {
        TopLeft: XYPos 
        BottomRight: XYPos 
    }

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"

let log2 x = 
    log(float(x))/log(2.)
    |> System.Math.Ceiling
    |> int

//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS() = 
    System.DateTime.Now.Ticks
    |> (fun t -> t /10L)

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats

//--------------------------------Constants----------------------------------//




/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions : XYPos = 
    {X = 1000. ; Y = 1000.}


//makes symbol bounding box (should be in symbol)
//assumes that the XYPos of Symbol is the top left of the box 
//size is how big you wnat the box to be -- makes it a square
//this depends on the symbol person anyway so you cna change it


//same as symbol 
//works for one segment

// let makeBox (w: Wire) (size: float) = 
//     failwithf "not yet done"



///checks which symbol has been clicked
//returns symbol 
//should be in symbol.fs


//checks which wire has been clicked
//returns wire
//should be in wire.fs

// let checkWire (mousePos: XYPos) (model: Model) = 
//     let wire = model.WX
//     let res = List.tryFind (fun w -> containsPoint (makeBox w 2.0) mousePos) wire


//------------------------ Bounding box functions -----------------------------
//let me know if you need any more 


//is a point inside the box 
let containsPoint  (box: BB) (point: XYPos) = 
    point.X >= box.TopLeft.X && point.Y >= box.TopLeft.Y && point.X <= box.BottomRight.X && point.Y<= box.BottomRight.Y


//dist between point and BB 
let distFromPoint (point: XYPos) (box: BB) = 
    let dist1 = ((point.X - box.TopLeft.X) ** 2.0) + ((point.Y - box.TopLeft.Y) ** 2.0)
    let dist2 = ((point.X - box.BottomRight.X) ** 2.0) + ((point.Y - box.BottomRight.Y) ** 2.0)
    min(dist1, dist2)


    



// module Helpers
// open Browser.Types
// open Fable.Core
// open Fable.Core.JsInterop
// open Electron
// open Fable.React

// //-------------------------------------------------------------------------//
// //------------------------------Types--------------------------------------//
// //-------------------------------------------------------------------------//

// /// position on SVG canvas
// type XYPos =
//     {
//         X : float
//         Y : float
//     }

// type MouseOp = 
//     /// button up
//     | Up
//     /// button down
//     | Down
//     /// Move with button up
//     | Move 
//     /// Move with button Down
//     | Drag

// type MouseT = 
//     {
//         Pos: XYPos
//         Op: MouseOp
//     }

// type BB = 
//     {
//         TopLeft: XYPos 
//         BottomRight: XYPos 
//     }

// //--------------------------------------------------------------------------//
// //-----------------------------Helpers--------------------------------------//
// //--------------------------------------------------------------------------//

// /// return a v4 (random) universally unique identifier (UUID)
// let uuid():string = import "v4" "uuid"


// //-----------------Code to record and print execution time statistics-------//

// let timeNowInMicroS() = 
//     System.DateTime.Now.Ticks
//     |> (fun t -> t /10L)

// type Stats = {
//     Min: float
//     Max: float
//     Av: float
//     Num: float
//     }

// /// add time t to st
// let addTimeToStats (t:float) (st:Stats) =
//     {
//         Min = min st.Min t
//         Max = max st.Max t
//         Av = (st.Av*st.Num + t)/(st.Num+1.)
//         Num = st.Num + 1.
//     }

// /// execution time stats indexed by name in recordExecutionStats
// let mutable executionStats = Map<string,Stats> []

// /// Run (f arg) recording its time in executionStats under name.
// /// NB - this will run f multiple times if needed to estimate average speed more accurately.
// /// If an execution time of 5ms for this function is too long reduce timeLimit.
// /// The multiple time execution will not work, and will give lower than real results, if
// /// f is memoised. In that case set timeLimit to 0. for only one execution.
// let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
//     let timeLimit = 0. // time in ms to execute f for.
//     let t1 = timeNowInMicroS()
//     let execTime() = float (timeNowInMicroS() - t1) / 1000.
//     let res = f arg // do f
//     let mutable iterations = 1
//     while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
//         iterations <- iterations + 1
//         f arg |> ignore // do f again
//     let t = execTime() / float iterations
//     executionStats <-
//         Map.tryFind name executionStats
//         |> Option.map (addTimeToStats t)
//         |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
//         |> (fun st -> Map.add name st executionStats)
//     res

// /// print
// let printStats() =
//     executionStats
//     |> Map.toList
//     |> List.iter (fun (name,st) -> 
//         printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
//     executionStats <- Map [] // reset stats

// //--------------------------------Constants----------------------------------//

// /// these determine the size of the canvas relative to the objects on it.
// let canvasUnscaledDimensions : XYPos = 
//     {X = 1000. ; Y = 1000.}


// //------------------------------My Helpers---------------------------------//

// let log2 x = 
//     log(float(x))/log(2.)
//     |> System.Math.Ceiling
//     |> int

// //let result = log2 9


// //--------------------------------Bounding Box functions----------------------------------// (Made by Anushka)


// //makes symbol bounding box (should be in symbol)
// //assumes that the XYPos of Symbol is the top left of the box 
// //size is how big you wnat the box to be -- makes it a square
// //this depends on the symbol person anyway so you cna change it


// // //same as symbol 
// // //works for one segment
// // let makeBox (w: Wire) (size: float) =     // *makeBox not yet implemented*
// //     failwithf "not yet done"


// //is a point inside the box 
// let containsPoint  (box: BB) (point: XYPos) = 
//     point.X >= box.TopLeft.X 
//     && point.X <= box.BottomRight.X 
//     && point.Y <= box.TopLeft.Y 
//     && point.Y >= box.BottomRight.Y


// //dist between point and BB 
// let distFromPoint (point: XYPos) (box: BB) = 
//     let dist1 = ((point.X - box.TopLeft.X) ** 2.0) + ((point.Y - box.TopLeft.Y) ** 2.0)
//     let dist2 = ((point.X - box.BottomRight.X) ** 2.0) + ((point.Y - box.BottomRight.Y) ** 2.0)
//     min(dist1, dist2)


// // ///checks which symbol has been clicked
// // //returns symbol 
// // //should be in symbol.fs
// // let findSymbol (mousePos: XYPos) (model: Model) =    // *waiting on implementation of makeBox*
// //     List.tryFind (fun sym -> containsPoint (makeBox sym 2.0) mousePos) model





    

