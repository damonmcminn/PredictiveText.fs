#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-import-fetch/Fable.Import.Fetch.fs"
#load "../node_modules/fable-import-fetch/Fable.Helpers.Fetch.fs"

open System
open System.Collections

open Fable.Core
open Fable
open Fable.Import.Fetch
open Fable.Helpers.Fetch

module Util =
    // map a List of single alphanum character strings to a list (letter, keycode) pairs
    let private xsToCodes index xs =
        List.map (fun letter -> (letter, (index + 2).ToString())) xs

    // build map of keypad codes
    let private keypadCodes =
        let populateMap (map : Map<string, string>) (letter, index) = map.Add(letter, index)
        [
            ["a"; "b"; "c"]
            ["d"; "e"; "f"]
            ["g"; "h"; "i"]
            ["j"; "k"; "l"]
            ["m"; "n"; "o"]
            ["p"; "q"; "r"; "s"]
            ["t"; "u"; "v"]
            ["w"; "x"; "y"; "z"]
        ]
        |> List.mapi xsToCodes
        |> List.concat
        // reduce to Map
        |> List.fold populateMap Map.empty

    let stringToListOfStrings str =
        str
        |> List.ofSeq
        |> List.map (fun c -> c.ToString())

    let calculateWordCode (word : string) =
        word.ToLower()
        |> String.collect (fun c -> keypadCodes.Item(c.ToString()))

module Trie =
    type Node(value : string, parent : Node option) =
        // mutable properties, but immutable collections
        let mutable children = Map.empty<string, Node>
        let mutable data = []

        // optional parent Node
        member this.Parent = parent
        // Map of child Nodes
        member this.Children = children
        member this.Value = value
        // List of strings mapped to this Node
        member this.Data = data
        // replace children with updated Map
        member this.AddChild(child : Node) = children <- children.Add(child.Value, child)
        member this.RemoveChild key = children <- children.Remove key
        // replace data with updated List
        member this.AddData(word : string) = data <- word :: data
        member this.RemoveData word = data <- List.filter (fun w -> w <> word) data

    type T9() =
        // code is simpler is simpler by having root node with a string value
        // for root value to be None, would have to pattern match when retrieving value
        let rootValue = "^"
        let rootNode = Node(rootValue, None)

        let rec insertWord (keyCodes : List<string>) (node : Node) (word : string) =
            match keyCodes with
            // no more keys remain, so add data to Node
            | [] ->
                node.AddData word |> ignore
                node
            // keys remain
            | keyCode :: tail ->
                let child = node.Children.TryFind keyCode
                let nodeForRecursiveCall =
                    match child with
                    | Some child -> child
                    | None ->
                        let newNode = Node(keyCode, Some node)
                        node.AddChild(newNode) |> ignore
                        newNode
                insertWord tail nodeForRecursiveCall word

        let rec findFromKeyCodes (keyCodes : List<String>) (node : Node) =
            match keyCodes with
            | [] -> node
            | keyCode :: tail ->
                let child = node.Children.TryFind keyCode
                match child with
                | Some child -> findFromKeyCodes tail child
                | None -> node

        let rec delete word (node : Node) =
            let parent = node.Parent
            match parent with
            // no parent? could not delete anything
            | None -> false
            | Some parent ->
                // remove word from data then check the updated data List
                node.RemoveData word |> ignore
                match node.Data with
                // if words remain, this node should not be deleted
                | _ :: _ -> true
                // if node has an empty data List and no children, it can be deleted
                | [] ->
                    if node.Children.IsEmpty then parent.RemoveChild node.Value
                    // recursive call to prune dead branches
                    delete word parent

        // PUBLIC METHODS
        member this.Insert word =
            let keyCodes = (Util.calculateWordCode >> Util.stringToListOfStrings) word
            insertWord keyCodes rootNode word |> ignore

        // type is inferred as seq<'T>
        member this.Find (key : string) =
            findFromKeyCodes (Util.stringToListOfStrings key) rootNode

        member this.Delete word =
            let node = Util.calculateWordCode word |> this.Find
            delete word node

async {
    let t9 = Trie.T9()

    try
        // download list of 20,000 most common English words and insert into t9
        let! fetched = fetchAsync("https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt", [])
        let! txt = fetched.text() |> Async.AwaitPromise

        txt.Split '\n'
        |> List.ofArray
        |> Seq.iter t9.Insert
        |> ignore

        // just for japes: calculate word codes from list of words, find in t9 and print list to console
        let words =
            [
                "me"
                "s"
                "services"
                "some"
                "these"
                "click"
                "its"
                "like"
                "service"
            ]
            |> List.map (Util.calculateWordCode >> t9.Find >> (fun node -> node.Data.Head))

        Console.WriteLine words
    with
    | error -> Console.WriteLine error
} |> Async.Start

// Import.Node.require.Invoke("core-js") |> ignore

// let d = Import.Browser.document

// let createKey num =
//     let div = d.createElement "div"
//     div.innerText <- num.ToString()
//     div

// let keypad = d.getElementById "keypad"

// [2..9] |> List.map createKey |> Seq.iter (fun div -> keypad.appendChild(div) |> ignore)