namespace PredictiveText.Core

open System
open System.Collections

module Trie =
    module Keymap =
        /// A Map of keypad codes
        let keypadCodesMap =
            [ [ 'a' .. 'c' ], 2
              [ 'd' .. 'f' ], 3
              [ 'g' .. 'i' ], 4
              [ 'j' .. 'l' ], 5
              [ 'm' .. 'o' ], 6
              [ 'p' .. 's' ], 7
              [ 't' .. 'v' ], 8
              [ 'w' .. 'z' ], 9 ]
            |> List.collect(fun (letters, keypad) -> letters |> List.map(fun l -> l, keypad))
            |> Map.ofList

        /// Generate the sequence of T9 key codes that map to a word
        let calculateWordCode (word:string) = word.ToLower() |> Seq.choose keypadCodesMap.TryFind |> Seq.toList

    /// Represents a node in a tree. Each node contains a set of valid words for this node plus any nested children. 
    type Node = { Words:string Set; Children:Map<int, Node> }

    /// An empty node.
    let emptyNode = { Words = Set.empty; Children = Map.empty }

    module NodeFunctions =
        /// Adds a child to a node with a specific key. 
        let addChild key child node = { node with Children = node.Children.Add(key, child) }
        /// Adds a word to a node.
        let addWord word node = { node with Words = node.Words.Add word }

    module T9 =
        open NodeFunctions
        open Keymap

        let rec private insertWord keyCodes word node =
            match keyCodes with
            | [] -> node |> addWord word
            | keyCode :: tail ->
                let child =
                    match node.Children.TryFind keyCode with
                    | Some child -> child
                    | None -> emptyNode
                    |> insertWord tail word

                node |> addChild keyCode child
        
        let rec private tryFindNode keyCodes node =
            match keyCodes with
            | [] -> Some node
            | keyCode :: tail ->
                match node.Children.TryFind keyCode with
                | Some child -> tryFindNode tail child
                | None -> None

        /// Inserts a single word to the supplied tree
        let insert tree word =
            let code = calculateWordCode word
            tree |> insertWord code word

        /// Tries to find all words for a given set of keycodes
        let suggest (key:string) tree =
            let codes = key |> Seq.map int |> Seq.toList
            tree
            |> tryFindNode codes
            |> Option.map(fun node -> node.Words)
            |> defaultArg
            <| Set.empty
            |> Set.toList
