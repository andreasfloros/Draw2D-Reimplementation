**HLP Project 2021**

Team 1

Authors: 
- Moin Bukhari (Sheet.fs)
- Andreas Floros (BusWire.fs)
- Zaid Jafarey (Symbol.fs)
- Anushka Kulkarni (Sheet.fs)
- Vasileios Manginas (BusWire.fs)
- Shaheer Mapara (Symbol.fs)

## Communication between modules for mouse clicks
Sheet will have a function called get_hit(point: XYPos) which will have as parameter the position that has been clicked on the canvas. This function will then query symbol and wire (in that order) to find out what has been clicked. If something has been clicked, then the unique id of that module will be returned to sheet, which will then send the mouse msgs accordingly. 

Symbol and wire will use the bounding box functions to perform a search to find out which symbol/wire has been clicked, and return that information to sheet.

## Common Function Definitions

### Bounding Box functions

Symbol and wire will create their own bounding boxes when they are created. 
the bounding box functions below will be written by sheet in Helpers.fs, and symbol and wire can access them during the search.

#### type definition:
type BB = 
{
  TopLeft: XYPos
  BottomRight: XYPos
}

#### distance between point and bounding box 
let distFromPoint (point: XYPos) (box: BB) 
returns float

#### a point inside the box 
let containsPoint  (box: BB) (point: XYPos) 
returns bool


