module CommonTypes

// Abstract Id
type PortId = string
type PortType = Input | Output

type Port = {
                PortNumber : int
                PortType : PortType
                PortWidth : int
                PortPos : Helpers.XYPos
                RelativePortPos : Helpers.Dir
            }

let createPort (portNumber : int) (portType : PortType) (portWidth : int) (portPos : Helpers.XYPos) (relativePortPos : Helpers.Dir) =
        {
            PortNumber = portNumber
            PortType = portType
            PortWidth = portWidth
            PortPos = portPos
            RelativePortPos = relativePortPos
        }

let getPosFromPort port = port.PortPos
let generatePortId () = Helpers.uuid()
let movePortToPos port pos = {port with PortPos = pos}
let movePortByPos port pos = {port with PortPos = Helpers.posAdd port.PortPos pos}
let getDirFromPort port = port.RelativePortPos
let getPortTypeFromPort port = port.PortType
let getWidthFromPort port = port.PortWidth

// Extensions have a fixed length of 10.
// This works for the demo and will probably be fine for later as well
let getPortExtension pt dir isOutput=
    let ext =
        match dir with
        | Helpers.Dir.Up -> Helpers.segOf pt (Helpers.posDiff pt (Helpers.posOf 0. 30.))
        | Helpers.Dir.Down -> Helpers.segOf pt (Helpers.posAdd pt (Helpers.posOf 0. 30.))
        | Helpers.Left -> Helpers.segOf pt (Helpers.posDiff pt (Helpers.posOf 30. 0.))
        | Helpers.Right -> Helpers.segOf pt (Helpers.posAdd pt (Helpers.posOf 30. 0.))
    if isOutput then ext else Helpers.swapSeg ext

type Color = |Red | Blue | Green | Grey
    with 
        member this.Text() =
            match this with
            | Red -> "Red"
            | Blue -> "Blue"
            | Green -> "Green"
            | Grey -> "Grey"
