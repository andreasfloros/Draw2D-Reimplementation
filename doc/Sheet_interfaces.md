# Functions: Helper functions

The following are provided by Sheet for common use and can be found in Helpers.fs
 
```
type BB = //bounding box
{
    TopLeft : XYPos
    BottomRight : XYPos
}

ContainsPoint (box: BB) (click: XYPos) : bool //checks whether the user has clicked within a bounding box 
```

# Functions: Symbol to Sheet 

The following are required by Sheet from Symbol.

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

# Functions: BusWire to Sheet

The following are required by Sheet from BusWire.

```
getSymbolModelFromWireModel (wireModel : BusWire.Model) : Symbol.Model // Get the symbol model from a wire model
wireHit (mousePos : XYPos) (wireModel : BusWire.Model) : (WireId * int) option // Get the wire segment that was clicked
```

# Features:

Sheet requires BusWire and Symbol modules to modify certain types in the module for correct implementation by Sheet. 

## BusWire 
These are the extra features added for symbol. 

### Messages 
```
type Msg = 
| DeleteWire of CommonTypes.ConnectionId
| DeleteSym of CommonTypes.SymbolId 
| Select of CommonTypes.ConnectionId
| Unselect of CommonTypes.ConnectionId
```

### Model 
```
type Wire = 
{
    Colour : CommonTypes.HighLightColor
}
```
## Symbol 
These are the extra features added for symbol. 

### Messages 
```
type Msg = 
| Unselect of CommonTypes.SymbolId
```

### Model 
```
type Symbol =
{
    IsSelected : bool 
    Colour : CommonTypes.HighlightColor 
}
```
