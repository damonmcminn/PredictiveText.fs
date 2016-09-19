#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-import-fetch/Fable.Import.Fetch.fs"
#load "../node_modules/fable-import-fetch/Fable.Helpers.Fetch.fs"

// must be loaded in alphabetical order
#load "Core.fs"
#load "Ui.fsx"

open System
open Fable.Core
open Fable.Import.Fetch
open Fable.Helpers.Fetch
open PredictiveText

Fable.Import.Node.require.Invoke("core-js") |> ignore

async {
    let t9 = Core.Trie.T9()

    try
        // download list of 20,000 most common English words and insert into t9
        let! fetched = fetchAsync("https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt", [])
        let! txt = fetched.text() |> Async.AwaitPromise

        // populate t9
        txt.Split '\n' |> List.ofArray |> Seq.iter t9.Insert |> ignore

        Ui.View(t9).InitializeView |> ignore
    with
    | error -> Console.WriteLine error
} |> Async.Start