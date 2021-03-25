module Renderer

    open Elmish
    open Elmish.React
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    
    open Helpers
    open Electron
    
  
    
    
    open Sheet


    
    let makeMenu (name : string) (table : MenuItemOptions list) =
       let subMenu = createEmpty<MenuItemOptions>
       subMenu.``type`` <-  MenuItemType.SubMenu
       subMenu.label <- name
       subMenu.submenu <- U2.Case1 (table |> Array.ofList)
       subMenu  
    
    let menuSeparator =
       let sep = createEmpty<MenuItemOptions>
       sep.``type`` <- MenuItemType.Separator
       sep

    let makeRoleItem (role:MenuItemRole) =
        jsOptions<MenuItemOptions> <| fun item ->
            item.role <- role

    let makeKeyItem (label:string) (accelerator : string) (action : unit -> unit) =
        jsOptions<MenuItemOptions> <| fun item ->
            item.label <- label
            item.accelerator <- accelerator
            item.click <- fun _ _ _ -> action()

    // Editor Keybindings (also items on Edit menu)
    // Use Elmish subscriptions to attach external source of events such as keyboard
    // shortcuts. According to electron documentation, the way to configure keyboard
    // shortcuts is by creating a menu.
    let editMenu dispatch =
        makeMenu "Edit" 
                  [makeKeyItem "Default" "CmdOrCtrl+S" (fun () -> dispatch CtrlS)
                   makeKeyItem "Delete Selected"  "Delete" (fun () -> dispatch DEL)
                   makeKeyItem "AutoRouteAll" "CmdOrCtrl+W" (fun () -> dispatch CtrlW)
                   makeKeyItem "Zoom In" "CmdOrCtrl+="  (fun () -> dispatch CtrlPlus)
                   makeKeyItem "Zoom Out" "CmdOrCtrl+-" (fun () -> dispatch CtrlMinus)
                   makeKeyItem "Rotate Symbol" "R" (fun () -> dispatch R)
                   makeKeyItem "Copy Symbol" "X" (fun () -> dispatch X)
                   makeKeyItem "AutoRoutWire" "W" (fun () -> dispatch W)
                   menuSeparator
                   makeKeyItem "Print Statistics" "Alt+Shift+Z" (fun () -> dispatch AltShiftZ)
                   makeRoleItem MenuItemRole.ForceReload
                   makeRoleItem MenuItemRole.Reload
                   makeRoleItem MenuItemRole.ToggleDevTools
                   ]


    let symMenu dispatch =
        makeMenu "New" 
                  [makeMenu "Symbol"                  
                      [makeKeyItem "Xnor" "A" (fun () -> dispatch A)
                       makeKeyItem "And" "B" (fun () -> dispatch B)
                       makeKeyItem "Not" "C" (fun () -> dispatch C)
                       makeKeyItem "Or"  "D" (fun () -> dispatch D)
                       makeKeyItem "Xor" "E" (fun () -> dispatch E)
                       makeKeyItem "Nand" "F" (fun () -> dispatch F)
                       makeKeyItem "Nor" "G" (fun () -> dispatch G)
                       makeKeyItem "Decode4" "H" (fun () -> dispatch H)]]

    
    let attachMenusAndKeyShortcuts dispatch =
        let sub dispatch =
            let menu = 
                [| 
                    editMenu dispatch 

                    symMenu dispatch
                
                |]       
                |> Array.map U2.Case1
                |> electron.remote.Menu.buildFromTemplate   
            menu.items.[0].visible <- Some true
            electron.remote.app.applicationMenu <- Some menu
    
        Cmd.map KeyPress (Cmd.ofSub sub)   

    let update' = fun msg -> recordExecutionTimeStats "Update" (Sheet.update msg)
    let view'  = recordExecutionTimeStats "View" Sheet.view
    let printMsg (msg:Msg) =
        match msg with
        | KeyPress key -> sprintf "%A" key
        | Wire (BusWire.Msg.Symbol (Symbol.Msg.MouseMsg symMouseMsg)) -> sprintf "SymbolMsg:%A"  symMouseMsg.Op
        | x -> sprintf "Other:%A" x

    let traceFn (msg:Msg) model = printfn "Msg=%A\n\n" (printMsg msg)
    // App
    Program.mkProgram Sheet.init update' view'
    |> Program.withReactBatched "app"
    |> Program.withSubscription attachMenusAndKeyShortcuts
    |> Program.withTrace traceFn
    //|> Program.withConsoleTrace
    |> Program.run
