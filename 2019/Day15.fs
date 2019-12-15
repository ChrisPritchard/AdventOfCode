module Day15

open System.IO
open System.Collections.Generic

let input = (File.ReadAllText ("./inputs/day15.txt")).Split ','

type Agent = {
    x: int
    y: int
    state: int64
    memstate: Dictionary<int64, int64>
}

let clone o nx ny i =
    let copiedMemstate =
        Seq.toArray o.memstate 
        |> Array.map (fun kv -> kv.Key, kv.Value) 
        |> Array.copy
        |> dict |> Dictionary<int64, int64>
        
    let io = Intcode.IO.create()
    io.write i
    Intcode.run 0L 0L copiedMemstate io |> ignore
    let state = io.read () |> snd
    { x = nx; y = ny; state = state; memstate = copiedMemstate }

let edges o (visited: HashSet<int * int>) =
    let dirs = 
        [
            o.x, o.y - 1, 1L
            o.x, o.y + 1, 2L
            o.x - 1, o.y, 3L
            o.x + 1, o.y, 4L
        ] |> List.filter (fun (x, y, _) -> not (visited.Contains (x, y)))
    dirs |> List.iter (fun (x, y, _) -> visited.Add (x, y) |> ignore)
    dirs 
    |> List.map (fun (x, y, i) -> clone o x y i) 
    |> List.filter (fun o -> o.state <> 0L)

let part1 () =

    let mem = Intcode.memFrom input
    let start = { x = 0; y = 0; state = 1L; memstate = mem }
    let visited = HashSet<int * int> ()

    BFS.run (fun a -> a.state = 2L) (fun a -> edges a visited |> Seq.ofList) start
    |> Option.defaultValue [] |> fun l -> List.length l - 1

let part2 () =

    let mem = Intcode.memFrom input
    let start = { x = 0; y = 0; state = 1L; memstate = mem }
    
    let visited = HashSet<int * int> ()
    let fullmap = HashSet<int * int> ([0, 0])

    let mutable target = (0, 0)
    let getedges a =
        let r = edges a visited
        for e in r do
            if e.state = 2L then target <- (e.x, e.y)
            else fullmap.Add (e.x, e.y) |> ignore
        Seq.ofList r

    BFS.run (fun _ -> false) getedges start |> ignore

    let adjacent openSpace (x, y) = 
        [|0, -1; 0, 1; -1, 0; 1, 0|]
        |> Array.map (fun (dx, dy) -> x + dx, y + dy)
        |> Array.filter (fun p -> Set.contains p openSpace)

    let rec oxygen edges remaining cnt =
        let next = edges |> Array.collect (adjacent remaining) |> Array.distinct
        
        //System.Console.CursorLeft <- 0
        //System.Console.CursorTop <- 0
        //System.Console.Write (string (cnt + 1))
        //for (x, y) in next do
        //    System.Console.CursorLeft <- 30 + x
        //    System.Console.CursorTop <- 30 + y
        //    System.Console.Write "O"
        //System.Console.ReadKey true |> ignore

        let remaining = (next, remaining) ||> Array.foldBack Set.remove
        if Set.count remaining = 0 then cnt + 1
        else
            oxygen next remaining (cnt + 1)

    //System.Console.Clear ()
    //for x = -30 to 30 do
    //    for y = -30 to 30 do
    //        System.Console.CursorLeft <- 30 + x
    //        System.Console.CursorTop <- 30 + y
    //        if (x, y) = target then System.Console.Write "O"
    //        elif fullmap.Contains (x, y) then System.Console.Write "."
    //        else System.Console.Write "#"

    fullmap.Remove target |> ignore
    oxygen [|target|] (Set.ofSeq fullmap) 0
    