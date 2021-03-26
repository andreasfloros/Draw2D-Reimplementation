# Symbol to BusWire

The following functions are provided in Symbol for use by the BusWire module:

```
getPosFromPort (port : Port) : XYPos // Returns the coordinates of a port
getDirFromPort (port : Port) : Dir // Returns the side of the symbol that the port is on (Up,Down,Left,Right)
getWidthFromPort (port : Port) : int // Returns the BusWidth of a port
getPortsFromId (symbolId : SymbolId) (symbolModel : Symbol.Model) : Map<PortId,Port> // Returns all the ports connected to the symbol with the specified Id
```

# Sheet Interfaces 

The following functions are provided in Symbol for use by the Sheet module:

```
// gets the bounding box for a Symbol
getSymbolBBox (symbol: Symbol) : BB

// gets the bounding box for a Port
createPortBB (port: Port) (x: float) : BB

// Returns a Port if the position of the mouse is within the port bounding box, else returns None 
FindPort (mousePos: XYPos) (model: Model) : option<Port * PortType>

// Returns a Symbol if the position of the mouse is within the Symbol bounding box, else returns None 
FindSymbol (mousePos: XYPos) (model: Model) : option<SymbolId>

// Looks for a Sheet-Symbol, used by the drag and drop menu
findSheetSymbol (mousePos: XYPos) (model: Model) : option<SymbolId>

```

# Issie Interfaces 

The following functions are provided in Symbol for interfacing with Issie:
```
// extracts a component with a specific Id from the Model
extractComponent (symModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component

// extracts Symbol list from the model Model
extractComponents (symModel: Model) : CommonTypes.Component list = symModel.SymModel

// Looks up a symbol using its Id
getsymbolFromSymbolId (symbolId: SymbolId) (symModel: Model) : Symbol
```


# Messages

The following messages are found by Symbol:

```

| MouseMsg of MouseT // Provides mouse info
| StartDragging of sId : CommonTypes.SymbolId * pagePos: XYPos // Used to initiate dragging
| Dragging of pagePos: XYPos // Used to actually change the position of the selected symbol
| EndDragging  // Ends dragging 
| Unselect of sId : CommonTypes.SymbolId // To unselect a component 
| AddSymbol of CompType: CommonTypes.ComponentType * label: string * pagePos: XYPos // To add a new symbol
| CopySymbol // To copy a Symbol
| DeleteSymbol // To delete the selected symbol
| RotateSymbol of sId:CommonTypes.SymbolId // To rotate the selected symbol
| MultipleSelect of sId : CommonTypes.SymbolId * pagePos: XYPos // For selecting multiple components
| MouseMove of pagePos : XYPos * PortSelect: Port Option // To detect Mouse Move, used by port animations
| Deselect // Deselects all 
| SelectEnclosed of p1: XYPos * p2: XYPos // For drag and drop
| CopySheetSymbol of sId: CommonTypes.SymbolId * pagePos: XYPos // For click and drop menu


```
