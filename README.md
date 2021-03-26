# Draw2D library re-implementation
This is a re-implementation of the [Draw2D](http://www.draw2d.org/draw2d/) library currently used in the digital circuit design application [Issie](https://github.com/tomcl/issie).

The main defect in Issie is the Javascript schematic drawing library Draw2D. This is very capable and large, but buggy. Also it is very slow. The aim of this project is to reimplement the parts of Draw2d used in Issie from scratch (the existing js code is irrelevant to this), with a pure F# implementation using the Elmish MVU (pure functional) web framework.


Team 1 Authors: 
- Moin Bukhari
- Andreas Floros
- Zaid Jafarey
- Anushka Kulkarni
- Vasileios Manginas
- Shaheer Mapara


This file contains general documentation for the project.

## Implemented Features
### Symbol-related features:
- Symbol dragging
- Symbol creation
- Symbol deletion
- Symbol copy
- Symbol rotation
- "Snap-to-symbol": Symbol alignment with other symbols
- Visible ports on hover near a specific symbol + outlines on ports when Hovering over a them
- Comprehensive animation on which nearby symbol ports are compatible for connection during wire creation

### Wire-related features:
- Wire creation
- Manual routing for wires
- Split segment for wires (Ctrl+LeftClick)
- Intelligent deletion of unecessary segments within wire
- Wire snap
- Rounded corners for wires

### General features:
- Click-drag-drop demo menu
- Multiple selection with LeftClick on symbols and wires with Shift pressed
- Multiple selection with click and drag selection box
- Multiple item move with Shift pressed and moving one of the selected items
- Multiple deletion with Del key
- Multiple item copy and paste
- Single undo (CtrlZ) and redo (CtrlY)
- Zoom
- Scroll

## Interface Documentation

Module specific interface documentation can be found in the doc folder. There is a seperate .md file for each module.


## Communication between modules for mouse clicks
Sheet will have a function called getHit(point: XYPos) which will have as parameter the position that has been clicked on the canvas. This function will then query symbol and wire (in that order) to find out what has been clicked. If something has been clicked, then the unique id of that module will be returned to sheet, which will then send the mouse msgs accordingly.

Symbol and wire will use the bounding box functions to perform a search to find out which symbol/wire has been clicked, and return that information to sheet.

## Selecting Items
(Only) Sheet will have a a SelectedItem value which will indicate what object has been selected, if any.
Symbols and wires will include their render props and appropriate functions will be written by the symbol and wire module leaders upon request to change an objects props.
Examples would be changing the color of an object, resizing the object (if we decide to implement something like that later on).
Combining symbol and wire functions into one should allow sheet to act on a high level, i.e. deciding to color an object without going into the details of whether it is a symbol or a wire.

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




