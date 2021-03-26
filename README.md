# Draw2D library re-implementation
This is a re-implementation of the [Draw2D](http://www.draw2d.org/draw2d/) library currently used in the digital circuit design application [Issie](https://github.com/tomcl/issie).

The main defect in Issie is the Javascript schematic drawing library Draw2D. This is very capable and large, but buggy. Also it is very slow. The aim of this project is to reimplement the parts of Draw2d used in Issie from scratch (the existing js code is irrelevant to this), with a pure F# implementation using the Elmish MVU (pure functional) web framework.


### Team 1 Authors: 
- Moin Bukhari
- Andreas Floros
- Zaid Jafarey
- Anushka Kulkarni
- Vasileios Manginas
- Shaheer Mapara


This file contains general documentation for the project.

## How to run
Clone the repo locally and run the code in the directory with one of two ways:

(1) Run build or npm run dev (for first time the former rather than the latter is mandatory) commands from a command line (obviously, you need to be in this directory to run these commands). Doing this, the code is compiled in JS by FABLE (F# to JS compiler), and run under electron. That way you have a working GUI and all the HTML javascript interface (for example SVG) works, with Node/electron libraries. 

(2) Build the code under Visual Studio under dotnet with ```dotnet fake build```. Doing this, the code will compile under dotnet, with dotnet libraries.

The code is designed so that it will compile OK both ways. But the Node library functions will only work under electron, and the dotnet (non-core) library functions only work under dotnet.

## Implemented Features
### Symbol-related features:
- Complete set of Components used by Issie
- Additional Components also available for example: MUX-n, DEMUX-n and Custom Component 
- Symbol dragging
- Symbol creation (through click-drag-drop menu/catalogue on the side)
- Symbol deletion
- Symbol copy
- Symbol rotation
- "Snap-to-symbol": Symbol alignment with other symbols
- Visible ports on hover near a specific symbol + outlines on ports when hovering over them
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
- Scroll

### Useful functionality commands
Mouse/Keyboard user actions | Corresponding effects in application
----------------------------|-------------------------------------
Shift(held)+LeftClick | Multiple item selection
Ctrl+LeftClick | Split segment at current mouse position
Ctrl+Z | Single step undo
Ctrl+Y | Single step redo
DEL | Delete selected items
X | Copy and paste selected symbols
R | Rotation of selected symbol


## Interface Documentation
Module specific interface documentation can be found in the doc folder. There is a seperate .md file for each module.


## Communication between modules for mouse clicks
Sheet has a function called getHit(point: XYPos) which has as parameter the position that has been clicked on the canvas. This function will then query Port, Wire, Symbol, then SheetSymbol respectively to find out what has been clicked. This priority was a design decision for optimal functionality. If something has been clicked, then the unique id of that module is returned to sheet, which then sends the mouse messages accordingly.

Symbol and wire will use the bounding box functions to perform a search to find out which symbol/wire has been clicked, and return that information to sheet.

## Selecting Items

Sheet has a SelectedItem value which indicates what object has been selected, if any. This includes: Symbol, Wire, Port, SheetSymbol, or NoItem. This is to ensure the correct message is sent to the selectedItem based on which one has been selected. For eg. if it is a Symbol then it would allow dragging; if it is a Port, it would allow for wires to be connected using click-and-drag.

Symbol and Wire have an IsSelected boolean that decides the state of their renderProps (attributes). Examples would be changing the color of an object, resizing the object (if we decide to implement something like that later on).

The advantage of having the information of SelectedItems in all 3 modules has made the implementation of many functionalities easier.

## Common Function Definitions

### Bounding Box functions

Symbol and wire will create their own bounding boxes when they are created. 
The bounding box functions can be found in Helpers.fs, along with many other Helper functions that are used by the modules to implement functionalities.

#### type definition:
type BB = 
{
  TopLeft: XYPos
  BottomRight: XYPos
}




