#r "../node_modules/fable-core/Fable.Core.dll"
#load "Core.fs"

namespace PredictiveText.Ui

open PredictiveText.Core
open Fable.Core

type Model(t9: Trie.T9) =
    let mutable code = ""

    member this.GetState =
        (t9.Find code).Data

    member this.AddCode newCode  =
        code <- code + newCode

    member this.RemoveCode =
        code <- code.[0..(code.Length - 2)]


type View(t9: Trie.T9) =
    let doc = Fable.Import.Browser.document
    let model = Model(t9)
    let wordListContainer = doc.getElementById "words"

    let updateUI state =
        let currentWordList = doc.querySelector "#words .wordlist"

        let ul = doc.createElement "ul"
        ul.classList.add "wordlist"

        state
        |> List.map (fun word ->
            let listItem = doc.createElement "li"
            listItem.innerText <- word
            listItem)
        |> Seq.iter (ul.appendChild >> ignore)

        wordListContainer.removeChild currentWordList |> ignore
        wordListContainer.appendChild ul |> ignore

    let handleKeypadClick num =
        model.AddCode num
        updateUI model.GetState

    let handleBackspace() =
        model.RemoveCode
        updateUI model.GetState

    let createKey num =
        let key = doc.createElement "button"
        key.innerText <- num
        key.addEventListener_click (fun _ -> handleKeypadClick num; null)
        key

    let createKeypad =
        let keypad = doc.createElement "div"
        [2..9]
        |> List.map ((fun num -> num.ToString()) >> createKey)
        |> Seq.iter (keypad.appendChild >> ignore)
        keypad

    member this.InitializeView () =
        doc.getElementById "keypad" |> (fun el -> (el.appendChild(createKeypad))) |> ignore
        doc.getElementById "backspace" |> (fun el ->
            el.addEventListener_click (fun _ -> handleBackspace(); null))
