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

    let result = BFS.run (fun a -> a.state = 2L) (fun a -> edges a visited |> Seq.ofList) start
    0
    