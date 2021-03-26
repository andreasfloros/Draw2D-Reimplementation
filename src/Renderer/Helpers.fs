module Helpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React
open Fable.React.Props

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
    | Up // button up
    | Down  // button down
    | Move // Move with button up
    | Drag // Move with button Down
    | Shift
    | Ctrl

type MouseT = {

    Pos: XYPos
    Op: MouseOp

    }

type BB = {

        TopLeft: XYPos 
        BottomRight: XYPos 
    }

type Dir = Up | Down | Left | Right

type Segment = {
    
    Start : XYPos
    End : XYPos
}

let portLength = 30.

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

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


//------------------------ Bounding box functions -----------------------------
let createSelectBox p1 p2 = 
     {
        TopLeft = p2
        BottomRight = p1
     }


//is a point inside the box 
let containsPoint  (box: BB) (point: XYPos) = 
    point.X >= box.TopLeft.X && point.Y >= box.TopLeft.Y && point.X <= box.BottomRight.X && point.Y<= box.BottomRight.Y


//dist between point and BB 
let distFromPoint (point: XYPos) (box: BB) = 
    let dist1 = ((point.X - box.TopLeft.X) ** 2.0) + ((point.Y - box.TopLeft.Y) ** 2.0)
    let dist2 = ((point.X - box.BottomRight.X) ** 2.0) + ((point.Y - box.BottomRight.Y) ** 2.0)
    min(dist1, dist2)

let sqre (s1:float) =
    s1 ** 2.0

let calcPointsDist (pos1: XYPos) (pos2: XYPos) =
    sqrt (sqre(pos1.X- pos2.X) + sqre(pos1.Y- pos2.Y))


let gridLines (W: int) (H: int) =
    let boxLen = 15
    let opacity = 0.15
    
    ([0..boxLen..W]
    |> List.map (fun x -> 
    line [
                            X1 (float x)
                            Y1 0.
                            X2 (float x)
                            Y2 H
                            Style [
                                    Stroke "Grey"
                                    StrokeWidth "1px"
                                    ZIndex -1
                                    Opacity opacity
                                  ]] [])
    )
    @ ([0..boxLen..H]
    |> List.map (fun x -> 
    line [
                            X1 0.
                            Y1 (float x)
                            X2 W
                            Y2 (float x)
                            Style [
                                    Stroke "Grey"
                                    StrokeWidth "1px"
                                    ZIndex -1
                                    Opacity opacity
                                  ]] [])
    )



//--------------------------- Buswire Helpers ------------------------------

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

let pointsAreCloseInX pt1 pt2 thresh=
    let diff = posDiff pt1 pt2
    diff.X * diff.X < thresh
let pointsAreCloseInY pt1 pt2 thresh=
    let diff = posDiff pt1 pt2
    diff.Y * diff.Y < thresh 
let pointsAreClose pt1 pt2 thresh=
    pointsAreCloseInX pt1 pt2 thresh && pointsAreCloseInY pt1 pt2 thresh

let posToString pos =
    sprintf "%f %f " pos.X pos.Y

let pointsAreCloseInDir dir pt1 pt2 thresh=
    if dir |> isHorizontalDir then pointsAreCloseInX pt1 pt2 thresh
    else pointsAreCloseInY pt1 pt2 thresh

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

// Extensions have a fixed length of 10.
// This works for the demo and will probably be fine for later as well
let getPortExtension pt dir isOutput=
    let ext =
        match dir with
        | Dir.Up -> segOf pt (posDiff pt (posOf 0. portLength))
        | Dir.Down -> segOf pt (posAdd pt (posOf 0. portLength))
        | Left -> segOf pt (posDiff pt (posOf portLength 0.))
        | Right -> segOf pt (posAdd pt (posOf portLength 0.))
    if isOutput then ext else swapSeg ext


let getDoublePortExtension pt dir isOutput=
    let ext =
        match dir with
        | Dir.Up -> segOf pt (posDiff pt (posOf 0. (2.*portLength)))
        | Dir.Down -> segOf pt (posAdd pt (posOf 0. (2.*portLength)))
        | Left -> segOf pt (posDiff pt (posOf (2.*portLength) 0.))
        | Right -> segOf pt (posAdd pt (posOf (2.*portLength) 0.))
    if isOutput then ext else swapSeg ext


let verticesToString firstPoint secondPoint thirdPoint = 
    " L " + (posToString firstPoint) + " Q " + (posToString secondPoint) + (posToString thirdPoint)


let segmentToCurve currentSegment nextSegment minimumLength =
    let currentSegmentDir = dirOfSeg currentSegment
    let nextSegmentDir = dirOfSeg nextSegment

    if currentSegmentDir = Right then
        let firstPoint = {X = currentSegment.End.X - minimumLength ; Y = currentSegment.End.Y}
        let secondPoint = currentSegment.End
        if nextSegmentDir = Up then
            let thirdPoint = {X = nextSegment.Start.X ; Y = nextSegment.Start.Y - minimumLength}
            verticesToString firstPoint secondPoint thirdPoint
        else
            let thirdPoint = {X = nextSegment.Start.X ; Y = nextSegment.Start.Y + minimumLength}
            verticesToString firstPoint secondPoint thirdPoint
        
    elif currentSegmentDir = Left then
        let firstPoint = {X = currentSegment.End.X + minimumLength ; Y = currentSegment.End.Y}
        let secondPoint = currentSegment.End
        if nextSegmentDir = Up then
            let thirdPoint = {X = nextSegment.Start.X ; Y = nextSegment.Start.Y - minimumLength}
            verticesToString firstPoint secondPoint thirdPoint
        else
            let thirdPoint = {X = nextSegment.Start.X ; Y = nextSegment.Start.Y + minimumLength}
            verticesToString firstPoint secondPoint thirdPoint

    elif currentSegmentDir = Up then
        let firstPoint = {X = currentSegment.End.X ; Y = currentSegment.End.Y + minimumLength }
        let secondPoint = currentSegment.End
        if nextSegmentDir = Left then
            let thirdPoint = {X = nextSegment.Start.X - minimumLength ; Y = nextSegment.Start.Y}
            verticesToString firstPoint secondPoint thirdPoint
        else
            let thirdPoint = {X = nextSegment.Start.X + minimumLength ; Y = nextSegment.Start.Y}
            verticesToString firstPoint secondPoint thirdPoint

    else
        let firstPoint = {X = currentSegment.End.X ; Y = currentSegment.End.Y - minimumLength }
        let secondPoint = currentSegment.End
        if nextSegmentDir = Left then
            let thirdPoint = {X = nextSegment.Start.X - minimumLength ; Y = nextSegment.Start.Y}
            verticesToString firstPoint secondPoint thirdPoint
        else
            let thirdPoint = {X = nextSegment.Start.X + minimumLength ; Y = nextSegment.Start.Y}
            verticesToString firstPoint secondPoint thirdPoint


let segmentsToRoundedString segments =
    let nonZeroSegments = List.filter (fun segment -> lenOfSeg segment <> 0.) segments

    nonZeroSegments
    |> List.mapi (fun index segment  -> if index <> (nonZeroSegments.Length-1) then
                                            let currentSegment = nonZeroSegments.[index]
                                            let nextSegment = nonZeroSegments.[index+1]
                                            let currentSegmentLength = lenOfSeg currentSegment
                                            let nextSegmentLength = lenOfSeg nextSegment
                                            let currentSegmentDirection = dirOfSeg currentSegment
                                            let nextSegmentDirection = dirOfSeg nextSegment


                                            let sameDirections = (currentSegmentDirection = nextSegmentDirection)
                                            let oppositeDirections = areOppositeDirs currentSegmentDirection nextSegmentDirection

                                            if sameDirections then 
                                                ""
                                            elif oppositeDirections then
                                                " L " + posToString segment.End
                                            else
                                                if ((currentSegmentLength<20.) || (nextSegmentLength<20.)) then
                                                    let minimumLength = min currentSegmentLength nextSegmentLength
                                                    segmentToCurve currentSegment nextSegment (minimumLength/2.)
                                                else
                                                    segmentToCurve currentSegment nextSegment 10.
                                                   
                                        else
  
                                            " L " + posToString segment.End)
    |> List.reduce (+)


let segmentsAreClose seg1 seg2 =
    let snapThresh = 30.
    dirOfSeg seg1 = dirOfSeg seg2 
    && lenOfSeg (segOf seg1.Start seg2.Start) > 0. 
    && pointsAreCloseInDir (somePerpendicularDir (dirOfSeg seg1)) seg1.Start seg2.Start snapThresh

// maybe outdated at time of reading this
let getNovelPortExtension pt dir isOutput  dist=
    let ext =
        match dir with
        | Dir.Up -> segOf pt (posDiff pt (posOf 0. dist))
        | Dir.Down -> segOf pt (posAdd pt (posOf 0. dist))
        | Left -> segOf pt (posDiff pt (posOf dist 0.))
        | Right -> segOf pt (posAdd pt (posOf dist 0.))
    if isOutput then ext else swapSeg ext