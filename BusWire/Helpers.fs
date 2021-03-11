module Helpers
open Fable.Core.JsInterop

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

type Dir = Up | Down | Left | Right

let areOppositeDirs dir1 dir2 =
    match dir1, dir2 with
    | Up, Down | Down, Up | Left, Right | Right, Left -> true
    | _ -> false

let arePerependicularDirs dir1 dir2 =
    match dir1, dir2 with
    | Up, Left | Up, Right | Down, Left | Down, Right | Left, Up | Left, Down | Right, Up | Right, Down -> true
    | _ -> false

let isHorizontalDir dir =
    match dir with
    | Left | Right -> true
    | _ -> false

let somePerpendicularDir dir =
    match dir |> isHorizontalDir with
    | true -> Dir.Down
    | false -> Left

let getOppositeDir dir =
    match dir with
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let pointsAreCloseInX pt1 pt2 =
    let diff = posDiff pt1 pt2
    diff.X * diff.X < 10. ** (-9.)
let pointsAreCloseInY pt1 pt2 =
    let diff = posDiff pt1 pt2
    diff.Y * diff.Y < 10. ** (-9.) 
let pointsAreClose pt1 pt2 =
    pointsAreCloseInX pt1 pt2 && pointsAreCloseInY pt1 pt2

let posToString pos =
    sprintf "%f %f " pos.X pos.Y

let pointsAreCloseInDir dir pt1 pt2 =
    if dir |> isHorizontalDir then pointsAreCloseInX pt1 pt2
    else pointsAreCloseInY pt1 pt2

type Segment = {
    Start : XYPos
    End : XYPos
}

let segOf pt1 pt2 =
    {Start = pt1
     End = pt2}

let swapSeg segment =
    {Start = segment.End
     End = segment.Start}

let lenOfSeg segment =
    let dff = posDiff segment.End segment.Start
    (dff.X * dff.X + dff.Y * dff.Y) ** (0.5)

// Gives the direction of a segment. Will fail if the segment is 0 length.
let dirOfSeg segment =
    if lenOfSeg segment = 0. then failwithf "dirOfSeg Error: Function called with a 0 length segment" // for debugging, should never happen
    else
        let diff = posDiff segment.End segment.Start
        if diff.X * diff.X < diff.Y * diff.Y then
            match diff.Y > 0. with
            | true -> Dir.Down
            | false -> Dir.Up
        else
            match diff.X > 0. with
            | true -> Right
            | false -> Left

let moveSegmentToX mousePos segment =
    segOf (posOf mousePos.X segment.Start.Y) (posOf mousePos.X segment.End.Y)
 
let moveSegmentToY mousePos segment =
    segOf (posOf segment.Start.X mousePos.Y) (posOf segment.End.X mousePos.Y)

let translateRoute newStart (segments: Segment list) =
    let diff = posDiff newStart segments.Head.Start
    segments
    |> List.map (fun seg -> segOf (posAdd seg.Start diff) (posAdd seg.End diff))


let swapRoute segments =
    segments
    |> List.map swapSeg
    |> List.rev

let segmentToString segment =
    posToString segment.Start + posToString segment.End

let segmentsToString segments =
    segments
    |> List.map segmentToString
    |> List.reduce (+)



type BoundingBox = {
    BottomRight: XYPos
    TopLeft: XYPos
}

let boxOf bottomRight topLeft = 
    {
        BottomRight = bottomRight
        TopLeft = topLeft
    }

let isInsideBBox mousePos bBox =
    let coordsMinusBottomRight = posDiff bBox.BottomRight mousePos
    let topLeftMinusCoords = posDiff mousePos bBox.TopLeft
    let isInFirstQuad pt = pt.X > 0.0 && pt.Y > 0.0
    isInFirstQuad coordsMinusBottomRight && isInFirstQuad topLeftMinusCoords

let bBoxesIntersect box1 box2 =
    let topRight = posOf box1.BottomRight.X  box1.TopLeft.Y
    let bottomLeft = posOf box1.TopLeft.X box1.BottomRight.Y 
    isInsideBBox box1.TopLeft box2 || isInsideBBox box1.BottomRight box2 || isInsideBBox topRight box2 || isInsideBBox bottomLeft box2

let getSegmentBBox segment =
    if lenOfSeg segment = 0. then boxOf segment.Start segment.Start // catch this to prevent the failwithf in dirOfSeg
    else    // this could definitely be rewritten in a better way
        match dirOfSeg segment with
        | Dir.Down -> boxOf (posAdd segment.End (posOf 5.0 5.0)) (posDiff segment.Start (posOf 5.0 5.0))
        | Dir.Up -> boxOf (posAdd segment.Start (posOf 5.0 5.0)) (posDiff segment.End (posOf 5.0 5.0))
        | Right -> boxOf (posAdd segment.End (posOf 5.0 5.0)) (posDiff segment.Start (posOf 5.0 5.0))
        | Left -> boxOf (posAdd segment.Start (posOf 5.0 5.0)) (posDiff segment.End (posOf 5.0 5.0))

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


/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"


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




    

