module Day15

open Common
open System.IO
open System.Collections.Generic

let input = (File.ReadAllText ("./inputs/day15.txt")).Split ','

let inputStream = TQueue()
let outputStream = TQueue()

let part1 () =

    let findResult target map = 
        let edges (x, y) = 
            [
                x - 1, y
                x + 1, y
                x, y - 1
                x, y + 1
            ] |> Seq.filter (fun (x, y) -> (Map.tryFind (x, y) map |> Option.defaultValue '#') <> '#')
        BFS.bfs ((=) target) edges (0, 0) |> Option.defaultValue [] |> Seq.length
    
    let rec runner (x, y) ip rb mem map = 
        System.Console.Clear ()
        System.Console.CursorLeft <- 0
        System.Console.CursorTop <- 0

        let render = 
            asString <| seq {
                yield '\n'
                for oy = 0 to 80 do
                    for ox = 0 to 80 do
                        if (ox - 40, oy - 40) = (x, y) then yield 'D'
                        else yield Map.tryFind (ox - 40, oy - 40) map |> Option.defaultValue '.'
                    yield '\n'
            }
        printfn "%s" render

        printfn "dir: "
        let next = 
            System.Console.ReadKey () 
            |> function
                | c when c.Key = System.ConsoleKey.UpArrow -> 1L
                | c when c.Key = System.ConsoleKey.DownArrow -> 2L
                | c when c.Key = System.ConsoleKey.LeftArrow -> 3L
                | c when c.Key = System.ConsoleKey.RightArrow -> 4L
                | _ -> failwith "invalid command"

        inputStream.Enqueue next
        let _, ip, rb, mem = intcodeRun ops ip rb mem (read inputStream) (outputStream.Enqueue)
        let res = outputStream.Dequeue ()
        let map, pos =
            match res, next with
            | r, n when r = t0 && n = t1 -> Map.add (x, y - 1) '#' map, (x, y)
            | r, n when r = t0 && n = t2 -> Map.add (x, y + 1) '#' map, (x, y)
            | r, n when r = t0 && n = t3 -> Map.add (x - 1, y) '#' map, (x, y)
            | r, n when r = t0 && n = t4 -> Map.add (x + 1, y) '#' map, (x, y)
            
            | r, n when (r = t1 || r = t2) && n = t1 -> Map.add (x, y - 1) '.' map, (x, y - 1)
            | r, n when (r = t1 || r = t2) && n = t2 -> Map.add (x, y + 1) '.' map, (x, y + 1)
            | r, n when (r = t1 || r = t2) && n = t3 -> Map.add (x - 1, y) '.' map, (x - 1, y)
            | r, n when (r = t1 || r = t2) && n = t4 -> Map.add (x + 1, y) '.' map, (x + 1, y)

            | _, _ -> map, (x, y)

        if res = t2 then
            findResult pos map
        else
            runner pos ip rb mem map

    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>

    runner (0, 0) t0 t0 mem Map.empty

let part2 () =

    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>

    0
    