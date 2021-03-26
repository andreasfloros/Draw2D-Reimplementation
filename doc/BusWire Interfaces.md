Where types are ambiguous (SymbolId, PortId, WireId) any definition will work.

# BusWire Messages

```
    | Symbol of Symbol.Msg
    | ManualRouting of wireId : WireId * segmentIndex : int * mousePos : XYPos
    | DeleteWire of wireId: WireId
    | SplitSegment of wireId : WireId * segmentIndex : int * mousePos : XYPos
    | Select of wireId : WireId
    | MultipleSelect of wireId : WireId
    | AutoRouteAll
    | AutoRouteWire of wireId: WireId
    | CreateWire of port1 : CommonTypes.Port * port2 : CommonTypes.Port
    | CreateSheetWire of port : CommonTypes.Port Option * pos : XYPos
    | DeleteSheetWire
    | SelectEnclosed of p1: XYPos * p2: XYPos
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
