#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "Ui.fsx"

open Fable.Core
open Fable.PowerPack
open PredictiveText.Ui

Fable.Import.Node.require.Invoke("core-js") |> ignore

async {
    let model = Model()
    let view = View(model)

    // download list of 20,000 most common English words and insert into t9
    let wordList = "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"

    let! words =
        promise {
            let! res = Fetch.tryFetch wordList []

            let! text =
                match res with
                | Result.Error err -> promise { return "" }
                | Result.Ok response -> response.text()

            return text
        }
        |> Async.AwaitPromise

    // populate Model
    words.Split '\n'
    |> List.ofArray
    |> model.Populate
    |> ignore

    // initialize View
    view.Init

} |> Async.StartImmediate