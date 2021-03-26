The below specifications assume no knowledge of Symbol or Sheet other than:
1. Symbol people own the Port type.
2. The port type includes enough information for the following to be determined: its position on the canvas, its relative position with respect to the symbol it's attached to and its width.

Where types are ambiguous (SymbolId, PortId, WireId) any definition will work.

# BusWire Messages

```
Symbol of Symbol.Msg
ManualRouting of wireId: WireId * segmentIndex: int * mousePos: XYPos // moves the specified wire segment
AutoRouteAll // Auto routes all wires from their starting positions
```

# Symbol to BusWire

The following functions are expected from Symbol for use in the BusWire module:

```
// Returns the list of ports for a given symbol
getPortsFromSymbol (symbol: Symbol) : list<Port> 

// Returns the coordinates of a port
getPosFromPort (port : Port) : XYPos 

// Returns a port's PortType (Input/Output)
getPortTypeFromPort (port : Port) : PortType 

// Returns a ports ID
getPortIdFromPort (port: Port) : string

// Returns the side of the symbol that the port is on (Up,Down,Left,Right)
getDirFromPort (port : Port) : Dir 

// Returns the BusWidth of a port
getWidthFromPort (port : Port) : int

// Returns all the ports connected to the symbol with the specified Id
getPortsFromId (symbolId : SymbolId) (symbolModel : Symbol.Model) : Map<PortId,Port> 

// Returns a list of all symbols that are selected (IsSelected = true)
getSelectedSymbolList (model : Model) : list<SymbolId> 

// Returns a list of all ports belonging to symbols that are selected
getPortsOfSelectedSymbolList (model : Model) : list<string> 

// Returns a map of all ports belonging to symbols that are selected, with the PortId used as the key
getPortsMapOfSelectedSymbolList (model : Model) : Map<string,Port>

```

# BusWire to Sheet

The following functions are provided from BusWire for use in the Sheet module:

```

// Get the symbol model from a wire model
getSymbolModelFromWireModel (wireModel : BusWire.Model) : Symbol.Model 

// Get the wire segment that was clicked
findWire (mousePos : XYPos) (wireModel : BusWire.Model) : (WireId * int) option 

// Get all the segments of a wire 
getSegmentsFromWire (wire: Wire) : Segments 

```
