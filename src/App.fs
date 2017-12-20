module FableAdvent

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Import.ReactDom
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Elmish
open Elmish.React
module R = Fable.Helpers.React

type Model =
    { Text : string []
      Max : int
      Index : int
      SinceLast : int
      TicksPerUpdate : int }

type Msg =
    | Faster
    | Slower
    | ReceivedText of string []
    | Tick

let update msg model =
    match msg with
    | Tick ->
        match model with
        | { TicksPerUpdate = tpu; SinceLast = sl } when sl >= tpu && tpu >= 0 ->
            { model with
                Index = min model.Max (model.Index + 1)
                SinceLast = 0 }
        | { TicksPerUpdate = tpu; SinceLast = sl } when sl >= (tpu * -1) && tpu < 0 ->
            { model with
                Index = max 0 (model.Index - 1)
                SinceLast = 0 }
        | _ ->
            { model with SinceLast = model.SinceLast + 1 }
    | Faster ->
        { model with TicksPerUpdate = model.TicksPerUpdate - 1 }
    | Slower ->
        { model with TicksPerUpdate = model.TicksPerUpdate + 1 }
    | ReceivedText text ->
        { model with
            Text = text
            Max = Array.length text - 1 }

let requestUri =
    [ "https://api.esv.org/v3/passage/text/?q=John%201"
      "&include-passage-references=false"
      "&include-first-verse-numbers=false"
      "&include-verse-numbers=false"
      "&include-footnotes=false"
      "&include-footnote-body=false"
      "&include-passage-horizontal-lines=false"
      "&include-heading-horizontal-lines=false"
      "&include-headings=false"
      ] |> String.concat ""

let authToken = "687d64857778e9200ab6e6e161794a82554ad665"

let toText (res : Response) =
    res.text()

let (|Val|_|) key = Map.ofSeq >> (Map.tryFind key)

let extractPassage (str : string) =
    let json = Json.ofString str
    match json with
    | Ok (Json.Object (Val "passages" (Json.Array [|Json.String first|]))) -> first
    | _ -> "Error"

let getText dispatch =
    fetch requestUri [ requestHeaders [ Authorization <| sprintf "Token %s" authToken ] ]
    |> Promise.bind toText
    |> Promise.map extractPassage
    |> Promise.iter (fun text -> text.Split(Array.empty<char>, StringSplitOptions.RemoveEmptyEntries)
                                 |> ReceivedText
                                 |> dispatch)

let triggerUpdate (dispatch : Msg -> unit) =
    window.setInterval((fun _ -> dispatch Tick), 100) |> ignore

let view model dispatch =
    match model.Text with
    | t when Array.isEmpty t ->
        R.div []
            [ R.div [] [R.str "Loading..."] ]
    | _ ->
        R.div [ ClassName "container" ]
            [ R.button [ OnClick (fun _ -> dispatch Faster) ] [ R.str "Faster" ]
              R.div [ ClassName "theText" ] [ R.str model.Text.[model.Index]  ]
              R.button [ OnClick (fun _ -> dispatch Slower) ] [ R.str "Slower" ]
              R.div [] [ R.str <| sprintf "Ticks Per Update: %d" model.TicksPerUpdate ] ]

let init () =
    { Text = Array.empty
      Max = 0
      Index = 0
      SinceLast = 0
      TicksPerUpdate = 10 }

Program.mkSimple init update (lazyView2 view)
|> Program.withSubscription (fun _ -> Cmd.ofSub getText)
|> Program.withSubscription (fun _ -> Cmd.ofSub triggerUpdate)
|> Program.withReact "react-element"
|> Program.run