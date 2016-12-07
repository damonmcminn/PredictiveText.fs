#r "../node_modules/fable-core/Fable.Core.dll"
#load "Core.fsx"

namespace PredictiveText.Ui

open PredictiveText.Core
open Fable.Core

/// Represents the current view model - the dictionary, plus currently entered code
type Model =
    { Tree : Trie.Node
      Code : string }

/// Handles view concerns - dom stuff.
type View (model) =
    let mutable model = model
    let doc = Fable.Import.Browser.document
    let wordListContainer = doc.getElementById "words"

    let updateUI() =
        let currentWordList = doc.querySelector "#words .wordlist"

        let ul = doc.createElement "ul"
        ul.classList.add "wordlist"

        Trie.T9.suggest model.Code model.Tree
        |> List.map (fun word ->
            let listItem = doc.createElement "li"
            listItem.innerText <- word
            listItem)
        |> Seq.iter (ul.appendChild >> ignore)

        wordListContainer.removeChild currentWordList |> ignore
        wordListContainer.appendChild ul |> ignore

    let handleBackspace() =
        model <- { model with Code = model.Code.[0..(model.Code.Length - 2)] }
        updateUI()

    let createKeypad() =
        let handleKeypadClick num =
            model <- { model with Code = model.Code + num }
            updateUI()

        let createKey num =
            let key = doc.createElement "button"
            key.innerText <- num
            key.addEventListener_click(fun _ -> handleKeypadClick num; null)
            key

        let keypad = doc.createElement "div"
        
        [ 2 .. 9 ]
        |> List.iter (string >> createKey >> keypad.appendChild >> ignore)
        
        keypad

    member this.Init() =
        doc.getElementById "keypad" |> (fun el -> el.appendChild (createKeypad())) |> ignore
        doc.getElementById "backspace" |> (fun el -> el.addEventListener_click (fun _ -> handleBackspace(); null))
