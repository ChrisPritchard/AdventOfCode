module Day03

open System.IO
open System.Collections.Generic

let input = File.ReadAllLines ("./inputs/day03.txt")

let part1 () =
    let wire1 = input.[0].Split(',')
    let wire2 = input.[1].Split(',')

    let rec follow (wire: string[]) (map: HashSet<int * int>) index x y =
        if index >= wire.Length then map
        else
            let ins = wire.[index]
            let dir = ins.[0]
            let mag = int ins.[1..]
            if dir = 'R' then
                for dx = x to x + mag do
                    map.Add (dx, y) |> ignore
                follow wire map (index + 1) (x + mag) y
            else if dir = 'L' then
                for dx = x to x - mag do
                    map.Add (dx, y) |> ignore
                follow wire map (index + 1) (x - mag) y
            else if dir = 'D' then
                for dy = y to y + mag do
                    map.Add (x, dy) |> ignore
                follow wire map (index + 1) x (y + mag)
            else// if dir = 'U' then
                for dy = y to y - mag do
                    map.Add (x, dy) |> ignore
                follow wire map (index + 1) x (y - mag)
    
    let wire1Map = follow wire1 (HashSet<int * int>()) 0 0 0
    let wire2Map = follow wire2 (HashSet<int * int>()) 0 0 0

    Set.intersect (Set.ofSeq wire1Map) (Set.ofSeq wire2Map)
    |> Set.remove (0, 0)
    |> Set.toSeq
    |> Seq.map (fun (x, y) -> abs x + abs y)
    |> Seq.min

let part2 () =
    let wire1 = input.[0].Split(',')
    let wire2 = input.[1].Split(',')

    let rec follow (wire: string[]) (map: HashSet<int * int>) (steps: Dictionary<int * int, int>) index x y s =
        if index >= wire.Length then map, steps
        else
            let ins = wire.[index]
            let dir = ins.[0]
            let mag = int ins.[1..]
            if dir = 'R' then
                for d = 0 to mag do
                    let p = x + d, y
                    map.Add p |> ignore
                    if not (steps.ContainsKey p) then steps.Add (p, s + d)
                follow wire map steps (index + 1) (x + mag) y (s + mag)
            else if dir = 'L' then
                for d = 0 to mag do
                    let p = x - d, y
                    map.Add p |> ignore
                    if not (steps.ContainsKey p) then steps.Add (p, s + d)
                follow wire map steps (index + 1) (x - mag) y (s + mag)
            else if dir = 'D' then
                for d = 0 to mag do
                    let p = x, y + d
                    map.Add p |> ignore
                    if not (steps.ContainsKey p) then steps.Add (p, s + d)
                follow wire map steps (index + 1) x (y + mag) (s + mag)
            else// if dir = 'U' then
                for d = 0 to mag do
                    let p = x, y - d
                    map.Add p |> ignore
                    if not (steps.ContainsKey p) then steps.Add (p, s + d)
                follow wire map steps (index + 1) x (y - mag) (s + mag)

    let wire1Map, wire1Steps = follow wire1 (HashSet<int * int>()) (Dictionary<int * int, int>()) 0 0 0 0
    let wire2Map, wire2Steps = follow wire2 (HashSet<int * int>()) (Dictionary<int * int, int>()) 0 0 0 0

    Set.intersect (Set.ofSeq wire1Map) (Set.ofSeq wire2Map)
    |> Set.remove (0, 0)
    |> Set.toSeq
    |> Seq.map (fun p -> wire1Steps.[p] + wire2Steps.[p])
    |> Seq.min