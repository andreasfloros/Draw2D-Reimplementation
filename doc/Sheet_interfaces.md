
# Functions: Symbol to Sheet 

The following are required by Sheet from Symbol.

```

// Returns a Port if the position of the mouse is within the port bounding box, else returns None 
FindPort (mousePos: XYPos) (model: Model) : option<Port * PortType>

// Returns a Symbol if the position of the mouse is within the Symbol bounding box, else returns None 
FindSymbol (mousePos: XYPos) (model: Model) : option<SymbolId>

// Looks for a Sheet-Symbol, used by the drag and drop menu
findSheetSymbol (mousePos: XYPos) (model: Model) : option<SymbolId>

```

# Functions: BusWire to Sheet

The following are required by Sheet from BusWire.

```

// Get the symbol model from a wire model
getSymbolModelFromWireModel (wireModel : BusWire.Model) : Symbol.Model 

// Get the wire segment that was clicked
findWire (mousePos : XYPos) (wireModel : BusWire.Model) : (WireId * int) option 

// Get all the segments of a wire 
getSegmentsFromWire (wire: Wire) : Segments 

```

# Sheet Messages

The following messsages are used by Sheet.

``` 

type KeyboardMsg =  
    | CtrlS | AltShiftZ | DEL | CtrlW | W | R | CtrlPlus | CtrlMinus | X | CtrlZ | CtrlY

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT

```
