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
Clone the repo locally and build and run the code in the directory with one of two ways:

(1) Build the code from a command line using ```build``` or ```npm run dev``` command (for the first time the former rather than the latter is mandatory). Note that you must run these commands in the root directory. Doing this, the code is compiled in JS by FABLE (F# to JS compiler), and run under electron. That way you have a working GUI and all the HTML javascript interface (for example SVG) works, with Node/electron libraries. 

(2) Build the code in Visual Studio under dotnet with ```dotnet fake build```. Doing this, the code will compile under dotnet, with dotnet libraries.
Run ```dotnet tool restore``` before ```dotnet fake build``` if it doesn't work as expected. After building with dotnet for the first time you can again build using ```npm run dev```. 

The code is designed so that it will compile fine using both ways. But it is worth noting that the Node library functions will only work under electron, and the dotnet (non-core) library functions only work under dotnet. Also note that the reason for using ```npm run dev``` is that it is a lot faster than build.

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
- Split segment for wires
- Intelligent deletion of unecessary segments within wire
- Wire snap
- Rounded corners for wires

### General features:
- Click-drag-drop demo menu
- Multiple selection
- Multiple item move (Shift key held)
- Multiple deletion
- Multiple item copy and paste
- Single undo and redo
- Scroll

### Useful functionality commands
Mouse/Keyboard user actions | Corresponding effects in application
----------------------------|-------------------------------------
LeftClick+Drag | Box creation for multiple item selection
Shift(held)+LeftClick | Multiple item selection
Shift(held)+LeftClick+Drag | Multiple item movement
Ctrl+LeftClick | Split segment at current mouse position
W | Autoroute single selected wire
Ctrl+W | Autoroute all wires
Ctrl+Z | Single step undo
Ctrl+Y | Single step redo
DEL | Delete selected items
X | Copy and paste selected symbols
R | Rotation of selected symbol

### Half-implemented features
- Zoom: Currently this has been commented out from the Sheet Messages (Ctrl+Minus/Ctrl+Plus) due to incomplete implementation. Ctrl+Minus and Ctrl+Plus do work by themselves with regards to zooming but when zoomed in or out other features (e.g. selection) don't work.
- Menu/Catalogue: This requires extension for using the complete set of available symbol . This has not yet been done as it wasn't required for demoing functionality. Symbols for this can be added in the Symbol init function in the list SheetSymbol.
- Small bugs: 
1. Creating a wire and then undoing with Ctrl+Z leaves the wire-creating animation on canvas. Can be worked around by either pressing redo to make the created wire reappear and delete that wire or by starting to create another wire by dragging on a port. Probably not hard to fix but figured out last moment.
2. Scroll without moving the mouse at all does not update mouse coordinates. Example: Scroll without moving the mouse, immediately start dragging to create selectionBox, view the scroll offset between actual mouse position and displayed mouse position. (Delay of one view call). Probably hard to recreate in practice due to condition to not move the mouse at all, but even if done, goes away after any view call (any action)

## Interface Documentation
General documentation for module interaction as well as module specific interface documentation can be found in the doc folder. There is a seperate .md file for each module besides a general_description.md file for more general/non-module-specific information.
