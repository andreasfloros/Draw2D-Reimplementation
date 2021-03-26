## Communication between modules for mouse clicks
Sheet has a function called getHit(point: XYPos) which has as parameter the position that has been clicked on the canvas. This function will then query Port, Wire, Symbol, then SheetSymbol respectively to find out what has been clicked. This priority was a design decision for optimal functionality. If something has been clicked, then the unique id of that module is returned to sheet, which then sends the mouse messages accordingly.

Symbol and BusWire modules will use the bounding box functions to perform a search to find out which symbol/wire has been clicked, and return that information to sheet.

## Selecting Items

Sheet has a SelectedItem value which indicates what object has been selected, if any. This includes: Symbol, Wire, Port, SheetSymbol, or NoItem. This is to ensure the correct message is sent to the selectedItem based on which one has been selected. For eg. if it is a Symbol then it would allow dragging; if it is a Port, it would allow for wires to be connected using click-and-drag.

Symbol and Wire have an IsSelected boolean that decides the state of their renderProps (attributes). Examples would be changing the color of an object, resizing the object (if we decide to implement something like that later on).

The advantage of having the information of SelectedItems in all 3 modules has made the implementation of many functionalities easier.



### Bounding Box functions

Symbol and wire will create their own bounding boxes when they are created. 
The bounding box functions can be found in Helpers.fs, along with many other Helper functions that are used by the modules to implement functionalities.

#### Type Definition:
type BB = 
{

  TopLeft: XYPos

  BottomRight: XYPos
  
}
