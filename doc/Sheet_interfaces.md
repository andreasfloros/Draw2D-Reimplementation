# Functions: Helper functions by Sheet

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
FindSymbol (mousePos: XYPos) (sModel: Model) : Option Symbol.Id //get the symbol Id that was clicked 
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
