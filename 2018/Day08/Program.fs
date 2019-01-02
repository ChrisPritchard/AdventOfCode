open System
open System.IO

type Node = {
    children: Node list
    metadata: int list
}

let rec parseNode input =
    match input with
    | nodes::metadata::rest ->
        let children, remaining =
            [1..nodes] |> List.fold (fun (nodes, remaining) _ -> 
                let node, newRemaining = parseNode remaining
                (nodes @ [node], newRemaining)) ([], rest)
        { children = children; metadata = remaining |> List.take metadata }, List.skip metadata remaining
    | _ -> failwith "invalid input"

let rec part1 node =
    List.sum node.metadata + List.sumBy part1 node.children

let rec part2 node =
    match node.children with
    | [] -> List.sum node.metadata
    | _ -> 
        let max = List.length node.children
        let sum = node.metadata |> List.sumBy (fun m -> if m > max then 0 else part2 node.children.[m - 1])
        sum

[<EntryPoint>]
let main _ =

    let input = 
        (File.ReadAllText "input.txt").Split([|' '|]) 
        |> Array.toList |> List.map (Int32.Parse)
    let tree, _ = parseNode input

    printfn "part 1: %i" <| part1 tree
    printfn "part 2: %i" <| part2 tree

    0