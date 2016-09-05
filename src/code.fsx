#r "../node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable
open System
open System.Collections

Import.Node.require.Invoke("core-js") |> ignore

let element = Import.Browser.document.getElementById "app"
element.innerText <- "Hello, World!"

let xsToCodes index xs =
    List.map (fun letter -> (letter, (index + 2).ToString())) xs

// this .NET BCL unavailable in Fable
// https://fable-compiler.github.io/docs/compatibility.html
// let d = System.Collections.Hashtable(26)


let keypadCodes =
    // initialize empty letter code dictionary
    let result = Generic.Dictionary<string, string>(26)
    [
        ["a"; "b"; "c"];
        ["d"; "e"; "f"];
        ["g"; "h"; "i"];
        ["j"; "k"; "l"];
        ["m"; "n"; "o"];
        ["p"; "q"; "r"; "s"];
        ["t"; "u"; "v"];
        ["w"; "x"; "y"; "z"]
    ]
    |> List.mapi xsToCodes
    |> List.concat
    // reduce to immutable Map
    // |> List.fold (fun (acc : Map<string, int>) (letter, index) -> acc.Add(letter, index)) Map.empty
    // no need for immutable Map, so use mutable Dictionary
    // each element is a tuple in form (letter, index)
    |> Seq.iter result.Add
    result

let calculateWordCode (word : string) =
    word.ToLower()
    |> String.collect (fun c -> keypadCodes.Item(c.ToString()))

let wordToCodeList (word : string) =
    word.ToLower()
    |> Seq.map (fun c -> keypadCodes.Item(c.ToString()))
    |> Seq.toList

let words = [
    "damon"; "Saz"; "accessor"
]

let wordCodePairs =
    words
    |> List.map calculateWordCode
    |> List.zip words

type Node(value : string, parent : Node option) =
    let mutable children = Map.empty<string, Node>
    let mutable data = []

    member this.Parent = parent

    member this.Children = children
    member this.Value = value
    member this.Data = data
    member this.AddChild(child : Node) = children <- children.Add(child.Value, child)
    member this.RemoveChild key = children <- children.Remove key
    member this.AddData(word : string) =data <- word :: data
    member this.RemoveData word =
        data <- List.filter (fun w -> w <> word) data
        data
    member this.HasData = not data.IsEmpty

type Trie() =
    let rootValue = "^"
    let rootNode = Node(rootValue, None)

    let stringToListOfStrings str =
        str
        |> List.ofSeq
        |> List.map (fun c -> c.ToString())

    let rec insert (xs : List<string>) (node : Node) (word : string) =
        match xs with
            | [] ->
                node.AddData word |> ignore
                node
            | prefix::tail ->
                let child = node.Children.TryFind prefix
                let c =
                    match child with
                        | Some child -> child
                        | None ->
                            let newNode = Node(prefix, Some node)
                            node.AddChild(newNode) |> ignore
                            newNode
                insert tail c word

    let rec find (xs : List<String>) (node : Node) =
        match xs with
            | [] -> node
            | prefix::tail ->
                let child = node.Children.TryFind prefix
                match child with
                    | Some child -> find xs.Tail child
                    | None -> node

    let rec delete word (node : Node) =
        let parent = node.Parent
        match parent with
            | None -> false
            | Some parent ->
                let data = node.RemoveData word
                match data with
                    // words remain in list
                    | _ :: _ -> true
                    // remove node from parent if node has no children
                    // recursively delete to prune dead branch until reaching a node with data
                    | [] ->
                        if node.Children.IsEmpty then parent.RemoveChild node.Value
                        delete word parent

    member this.Insert word =
        let xs = word |> calculateWordCode |> stringToListOfStrings
        insert xs rootNode word |> ignore

    member this.Find key =
        let xs = key |> stringToListOfStrings
        find xs rootNode

    member this.Delete word =
        let key = word |> calculateWordCode
        let node = this.Find key
        delete word node


let t9 = Trie()

t9.Insert "aaaa"
t9.Insert "az"
t9.Insert "bbbb"

Console.WriteLine(t9.Delete("aaaa"))

Console.WriteLine(t9.Find(""))