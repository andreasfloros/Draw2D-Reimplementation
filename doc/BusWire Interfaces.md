The below specifications assume no knowledge of Symbol or Sheet other than:
1. Symbol people own the Port type.
2. The port type includes enough information for the following to be determined: its position on the canvas, its relative position with respect to the symbol it's attached to and its width.

# Symbol to BusWire

The following functions are expected from Symbol for use in the BusWire module:

```
getPosFromPort (port : Port) : XYPos // Get the coordinates of the port
getDirFromPort (port : Port) : Dir // Get the relative position of the port with respect to the symbol (Up,Down,Left,Right)
getWidthFromPort (port : Port) : int // Get the width of the port (definition will likely change later, for now assume this will only be called when the width is a constant)
getPortsFromId (symbolId : SymbolId) (symbolModel : Symbol.Model) : Map<PortId,Port> // Get all the ports connected to the symbol with the specified Id
```

# BusWire to Sheet

The following functions are provided from BusWire for use in the Sheet module:

```
getSymbolModelFromWireModel (wireModel : BusWire.Model) : Symbol.Model // Get the symbol model from a wire model
wireHit (mousePos : XYPos) (wireModel : BusWire.Model) : (BusWire.WireId * int) option // Get the wire segment that was clicked
```
