# Symbol to BusWire

The following functions are provided in Symbol for use by the BusWire module:

```
getPosFromPort (port : Port) : XYPos // Returns the coordinates of a port
getDirFromPort (port : Port) : Dir // Returns the side of the symbol that the port is on (Up,Down,Left,Right)
getWidthFromPort (port : Port) : int // Returns the BusWidth of a port
getPortsFromId (symbolId : SymbolId) (symbolModel : Symbol.Model) : Map<PortId,Port> // Returns all the ports connected to the symbol with the specified Id
```

# Functions: Symbol to Sheet 

The following functions are provided in Symbol for use by the Sheet module:

```
FindSymbol (mousePos: XYPos) (sModel: Model) : Option Symbol.Id // Returns the Id of the symbol found on a given XYPos. If no symbol is found, None is returned.
```


# Messages

The following messages are found by Symbol:

```
| MouseMsg of MouseT // Provides mouse info
| StartDragging of sId : CommonTypes.SymbolId * pagePos: XYPos // Used to initiate dragging
| Dragging of sId : CommonTypes.SymbolId * pagePos: XYPos // Used to actually change the position of the selected symbol
| EndDragging of sId : CommonTypes.SymbolId // Ends dragging 
| Unselect of sId : CommonTypes.SymbolId // To unselect components (provided by Sheet people)
| AddCircle of label: string * pagePos: XYPos // Used by demo code to add a circle
| AddSymbol of CompType: CommonTypes.ComponentType * label: string * pagePos: XYPos // Used to add a new symbol
| DeleteSymbol of sId:CommonTypes.SymbolId // Used to delete the selected symbol
| RotateSymbol of sId:CommonTypes.SymbolId  // Used to rotate the selected symbol
| UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
```
